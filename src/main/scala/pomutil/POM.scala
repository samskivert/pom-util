//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import java.io.File
import java.util.regex.{Pattern, Matcher}

import scala.collection.mutable.{ArrayBuffer, Set => MSet}
import scala.xml.{XML, Node}

/**
  * Project metadata.
  */
class POM (
  val parent :Option[POM],
  val parentDep :Option[Dependency],
  val file :Option[File],
  elem :Node
) {
  import POM._

  lazy val modelVersion :String = attr("modelVersion") getOrElse("4.0.0")

  lazy val groupId    :String = iattr("groupId", _.groupId)
  lazy val artifactId :String = attr("artifactId") getOrElse("missing")
  lazy val version    :String = iattr("version", _.version)
  lazy val packaging  :String = attr("packaging") getOrElse("jar")

  lazy val name        :Option[String] = attr("name")
  lazy val description :Option[String] = attr("description")
  lazy val url         :Option[String] = attr("url")

  lazy val scm :SCM = (elem \ "scm") match {
    case Seq(scm) => SCM(attr(scm, "connection"),
                         attr(scm, "developerConnection"),
                         attr(scm, "url"))
    case        _ => SCM(None, None, None)
  }

  lazy val modules    :Seq[String]  = (elem \ "modules" \\ "module") map(_.text.trim)
  lazy val profiles   :Seq[Profile] = (elem \ "profiles" \\ "profile") map(new Profile(this, _))
  lazy val properties :Map[String,String] =
    (elem \ "properties" \ "_") map(n => (n.label.trim, n.text.trim)) toMap

  lazy val depends :Seq[Dependency] = manageDepends(
    (elem \ "dependencies" \ "dependency") map(Dependency.fromXML(subProps)))
  lazy val dependMgmt :Map[String,Dependency] =
    (elem \ "dependencyManagement" \ "dependencies" \ "dependency") map(
      Dependency.fromXML(subProps)) map(d => (d.mgmtKey, d)) toMap

  /** Returns the depends in this POM and any depends inherited from its parents. */
  def fullDepends :Seq[Dependency] = parent.map(_.fullDepends).getOrElse(Seq()) ++ depends

  /** Build properties like `sourceDirectory` and other simple stuff. */
  lazy val buildProps :Map[String,String] =
    (elem \ "build" \ "_") filter(n => knownBuildProps(n.label.trim)) map(
      n => (n.label.trim, n.text.trim)) toMap // TODO: extract deeper stuffs

  /** Returns an identifier that encompases the group, artifact and version. */
  def id :String = groupId + ":" + artifactId + ":" + version

  /** Returns all the modules in the specified profile. */
  def modules (profileId :String) :Seq[String] =
    modules ++ profiles.find(_.id == profileId).toSeq.flatMap(_.modules)

  /** Returns all modules defined in the main POM and in all profiles. */
  def allModules :Seq[String] = (modules ++ profiles.flatMap(_.modules)).distinct

  /** Returns the dependencies in this POM, assuming the specified profile is active. */
  def depends (profileId :String) :Seq[Dependency] =
    depends ++ profiles.find(_.id == profileId).toSeq.flatMap(_.depends)

  /** Returns all dependencies defined in the main POM and in all profiles. */
  def allDepends :Seq[Dependency] = (depends ++ profiles.flatMap(_.depends)).distinct

  /** Returns the file for the top-most POM in the multimodule project of which this POM is a part.
    * This will return `None` if the POM was loaded from the .m2 repository. If this POM is not
    * part of a multimodule project (but was not loaded from the .m2 repository), it will return
    * itself as the top-most POM. */
  def rootPOM :Option[File] = parent flatMap(_ rootPOM) orElse (file match {
    case Some(f) if (!Dependency.isRepoFile(f)) => file
    case _ => None
  })

  // TODO: other bits

  /** Looks up a POM attribute, which may include properties defined in the POM as well as basic
    * project attributes like `project.version`, etc. */
  def getAttr (name :String) :Option[String] =
    // TODO: support env.x properties?
    // TODO: avoid infinite loop if `properties` map contains cycles
    getProjectAttr(name) orElse getEnvAttr(name) orElse properties.get(name).map(subProps) orElse
      parent.flatMap(_.getAttr(name))

  /** Returns a dependency on the (optionally classified) artifact described by this POM. */
  def toDependency (classifier :Option[String] = None,
                    scope :String = Dependency.DefaultScope,
                    optional :Boolean = false) =
    Dependency(groupId, artifactId, version, packaging, classifier, scope, None, optional)

  /** Returns true if this POM declares a snapshot artifact, false otherwise. */
  def isSnapshot = version.endsWith("-SNAPSHOT")

  /** Extracts the text of an attribute from the supplied element and substitutes properties. */
  def attr (elem :Node, name :String) :Option[String] = XMLUtil.text(elem, name) map(subProps)

  @deprecated("Use DependResolver", "0.7")
  def transitiveDepends (forTest :Boolean) :Seq[Dependency] =
    new DependResolver(this).resolve(forTest)

  /** A function that substitutes this POM's properties into the supplied text. */
  val subProps = (text :String) => {
    val m = PropRe.matcher(text)
    val sb = new StringBuffer
    while (m.find()) {
      val name = m.group(1)
      m.appendReplacement(sb, Matcher.quoteReplacement(getAttr(name).getOrElse("$!{" + name + "}")))
    }
    m.appendTail(sb)
    sb.toString
  }

  override def toString = groupId + ":" + artifactId + ":" + version + parent.map(
    " <- " + _).getOrElse("")

  private def iattr (name :String, pfunc :POM => String) =
    attr(name) orElse(parent map pfunc) getOrElse("missing")

  private def getProjectAttr (key :String) :Option[String] =
    if (!key.startsWith("project.")) None else key.substring(8) match {
      case "groupId" => Some(groupId)
      case "artifactId" => Some(artifactId)
      case "version" => Some(version)
      case "packaging" => Some(packaging)
      case "name" => name
      case "description" => description
      case "url" => url
      case pkey if (pkey.startsWith("parent.")) => pkey.substring(7) match {
        case "groupId" => parentDep.map(_.groupId)
        case "artifactId" => parentDep.map(_.artifactId)
        case "version" => parentDep.map(_.version)
        case _ => None
      }
      case _ => None
    }

  private def getEnvAttr (key :String) :Option[String] = Option(System.getProperty(key))

  // replaces any depends with "canonical" version from depmgmt section
  private def manageDepends (depends :Seq[Dependency]) :Seq[Dependency] = {
    val managed = depends map(d => dependMgmt.get(d.mgmtKey) match {
      case Some(md) => md.copy(scope = d.scope, optional = d.optional)
      case None     => d
    })
    (parent :\ managed)(_ manageDepends _)
  }

  private def attr (name :String) :Option[String] = attr(elem, name)
}

object POM {
  import XMLUtil._

  /** Contains the contents of the `<scm>` group. */
  case class SCM (
    val connection    :Option[String],
    val devConnection :Option[String],
    val url           :Option[String]
  )

  /** Parses the POM in the specified file. */
  def fromFile (file :File) :Option[POM] = fromXML(XML.loadFile(file), Some(file.getAbsoluteFile))

  /** Parses a POM from the supplied XML. */
  def fromXML (node :Node, file :Option[File]) :Option[POM] = node match {
    case elem if (elem.label == "project") => {
      val parentDep = (elem \ "parent").headOption map(Dependency.fromXML) map(
        _.copy(`type` = "pom"))
      val parentPath = (elem \ "parent" \ "relativePath").headOption map(_.text.trim) getOrElse(
        ".." + File.separator + "pom.xml")
      val parentFile = file map(f => new File(f.getParentFile, parentPath).getCanonicalFile)
      val parent = try {
        localParent(parentFile, parentDep) orElse installedParent(parentDep)
      } catch {
        case e :Throwable =>
          warn("Failed to read parent pom (" + parentDep + "): " + e.getMessage) ; None
      }
      // if we have a parent dep but were unable to find the parent POM, issue a warning
      if (parentDep.isDefined && !parent.isDefined) {
        warn(text(elem, "artifactId").getOrElse("unknown") + " missing parent: " + parentDep.get.id)
      }
      Some(new POM(parent, parentDep, file, elem))
    }
    case _ => None
  }

  /** Resolves the transitive set of all POMs included in the multi-module project rooted at `top`.
    * @return a mapping from dependency `id` to `POM` for this POM and all of its submodules. */
  def allModules (top :File) :Map[String,POM] = fromFile(top) match {
    case None => Map()
    case Some(pom) => {
      val topDir = top.getParentFile
      Map(pom.id -> pom) ++ pom.allModules.flatMap(
        m => allModules(new File(new File(topDir, m), "pom.xml")))
    }
  }

  private val knownBuildProps = Set("sourceDirectory", "testSourceDirectory")

  private def localParent (file :Option[File], parentDep :Option[Dependency]) = for {
    pdep <- parentDep
    parentFile <- file
    if (parentFile.exists)
    pom <- fromFile(parentFile)
    if (pom.toDependency() == pdep)
  } yield pom

  private def installedParent (parentDep :Option[Dependency]) = for {
    pdep <- parentDep
    pfile <- pdep.localPOM
    pom <- fromFile(pfile)
  } yield pom

  private def warn (msg :String) {
    Console.err.println("!!! " + msg)
  }

  private val PropRe = Pattern.compile("\\$\\{([^}]+)\\}")
}
