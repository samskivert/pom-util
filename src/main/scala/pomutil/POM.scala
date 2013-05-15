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
  lazy val packaging  :String = iattr("packaging", _.packaging)

  lazy val name        :Option[String] = attr("name")
  lazy val description :Option[String] = attr("description")
  lazy val url         :Option[String] = attr("url")

  lazy val modules    :Seq[String]  = (elem \ "modules" \\ "module") map(_.text.trim)
  lazy val profiles   :Seq[Profile] = (elem \ "profiles" \\ "profile") map(new Profile(this, _))
  lazy val properties :Map[String,String] =
    (elem \ "properties" \ "_") map(n => (n.label.trim, n.text.trim)) toMap

  lazy val depends :Seq[Dependency] = manageDepends(
    (elem \ "dependencies" \ "dependency") map(Dependency.fromXML(subProps)))
  lazy val dependMgmt :Map[String,Dependency] =
    (elem \ "dependencyManagement" \ "dependencies" \ "dependency") map(
      Dependency.fromXML(subProps)) map(d => (d.mgmtKey, d)) toMap

  /** Build properties like `sourceDirectory` and other simple stuff. */
  lazy val buildProps :Map[String,String] =
    (elem \ "build" \ "_") filter(n => knownBuildProps(n.label.trim)) map(
      n => (n.label.trim, n.text.trim)) toMap // TODO: extract deeper stuffs

  /** Returns an identifier that encompases the group, artifact and version. */
  def id = groupId + ":" + artifactId + ":" + version

  /** Returns all modules defined in the main POM and in all profiles. */
  def allModules = modules ++ profiles.flatMap(_.modules)

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
    // TODO: support env.x and Java system properties?
    getProjectAttr(name) orElse properties.get(name) orElse parent.flatMap(_.getAttr(name))

  /** Returns a dependency on the (optionally classified) artifact described by this POM. */
  def toDependency (classifier :Option[String] = None,
                    scope :String = Dependency.DefaultScope,
                    optional :Boolean = false) =
    Dependency(groupId, artifactId, version, packaging, classifier, scope, optional)

  /** Returns true if this POM declares a snapshot artifact, false otherwise. */
  def isSnapshot = version.endsWith("-SNAPSHOT")

  /** Extracts the text of an attribute from the supplied element and substitutes properties. */
  def attr (elem :Node, name :String) :Option[String] = XMLUtil.text(elem, name) map(subProps)

  /** Computes this POM's transitive dependencies. Exclusions are honored, and conflicts are resolved
    * using the standard "distance from root POM" Maven semantics. Direct dependencies with scopes
    * other than `compile` and `test` are included for this project, but not transitively, also per
    * standard Maven semantics.
    *
    * If this POM is part of a multi-module project, sibling dependencies will be resolved via the
    * POMs in sibling directories rather than via the .m2 repository. This differs from Maven, but
    * is vastly more useful and I wish Maven did things this way.
    *
    * @param forTest whether to include test dependencies.
    */
  def transitiveDepends (forTest :Boolean) :Seq[Dependency] = {
    // if we're part of a multimodule project, we want to resolve our "sibling" dependencies from
    // their neighboring directories rather than looking for them in the .m2 repository
    val sibDeps = rootPOM map(POM.allModules) getOrElse(Map())

    val haveDeps = MSet[(String,String)]()
    val allDeps = ArrayBuffer[Dependency]()
    def key (d :Dependency) = (d.groupId, d.artifactId)

    // we expand our dependency tree one "layer" at a time; we start with the depends at distance
    // one from the project (its direct dependencies), then we compute all of the direct
    // dependencies of those depends (distance two) and filter out any that are already satisfied
    // (this enforces the "closest to the root POM" conflict resolution strategy), then we repeat
    // the process, expanding to distance three and so forth, until we discover no new depends

    // TODO: handle exclusions

    def extract (deps :Seq[Dependency], mapper :(Dependency => Dependency)) {
      haveDeps ++= (deps map key)
      allDeps ++= deps
      val newdeps = for { dep <- deps
                          // if this depend is a sibling, use the "real" POM, otherwise get it from
                          // the .m2 repository
                          pom <- (sibDeps.get(dep.id) orElse dep.localPOM.flatMap(fromFile)).toSeq
                          dd <- pom.depends filterNot(d => dep.exclusions((d.groupId, d.artifactId)))
                          if (dd.scope == "compile" && !dd.optional && !haveDeps(key(dd)))
                        } yield mapper(dd)
      // we might encounter the same dep from two parents, so we .distinct to consolidate
      if (!newdeps.isEmpty) extract(newdeps.distinct, mapper)
    }

    val (compDeps, testDeps) = depends partition(_.scope != "test")
    extract(compDeps, d => d)
    if (forTest) extract(testDeps, _.copy(scope="test"))

    allDeps.toSeq
  }

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
