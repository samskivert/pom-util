//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import java.io.File
import java.util.regex.Pattern

import scala.collection.mutable.{ArrayBuffer, Set => MSet}
import scala.xml.{XML, Node}

/**
  * Project metadata.
  */
class POM (
  val parent :Option[POM],
  val parentDep :Option[Dependency],
  elem :Node
) {
  import POM._

  lazy val modelVersion :String = attr("modelVersion") getOrElse("4.0.0")

  lazy val groupId :String = attr("groupId") orElse(parent map(_.groupId)) getOrElse("missing")
  lazy val artifactId :String = attr("artifactId") getOrElse("missing")
  lazy val version :String = attr("version") orElse(parent map(_.version)) getOrElse("missing")
  lazy val packaging :String = attr("packaging") orElse(parent map(_.packaging)) getOrElse("missing")

  lazy val name :Option[String] = attr("name")
  lazy val description :Option[String] = attr("description")
  lazy val url :Option[String] = attr("url")

  lazy val properties :Map[String,String] =
    (elem \ "properties" \ "_") map(n => (n.label.trim, n.text.trim)) toMap
  lazy val depends :Seq[Dependency] =
    (elem \ "dependencies" \ "dependency") map(Dependency.fromXML(subProps))
  lazy val modules :Seq[String] =
    (elem \ "modules" \\ "module") map(_.text.trim)
  lazy val profiles :Seq[Profile] =
    (elem \ "profiles" \\ "profile") map(new Profile(this, _))

  /** Returns an identifier that encompases the group, artifact and version. */
  def id = groupId + ":" + artifactId + ":" + version

  /** Returns all modules defined in the main POM and in all profiles. */
  def allModules = modules ++ profiles.flatMap(_.modules)

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
    * @param forTest whether to include test dependencies.
    */
  def transitiveDepends (forTest :Boolean) :Seq[Dependency] = {
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
                          pom <- dep.localPOM.flatMap(fromFile).toSeq
                          dd <- pom.depends
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
      m.appendReplacement(sb, getAttr(name).getOrElse("\\$!{" + name + "}"))
    }
    m.appendTail(sb)
    sb.toString
  }

  override def toString = groupId + ":" + artifactId + ":" + version + parent.map(
    " <- " + _).getOrElse("")

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
      val parent = try {
        localParent(file, parentDep) orElse installedParent(parentDep)
      } catch {
        case e :Throwable =>
          warn("Failed to read parent pom (" + parentDep + "): " + e.getMessage) ; None
      }
      // if we have a parent dep but were unable to find the parent POM, issue a warning
      if (parentDep.isDefined && !parent.isDefined) {
        warn(text(elem, "artifactId").getOrElse("unknown") + " missing parent: " + parentDep.get.id)
      }
      Some(new POM(parent, parentDep, elem))
    }
    case _ => None
  }

  private def localParent (file :Option[File], parentDep :Option[Dependency]) = for {
    pdep <- parentDep
    pomFile <- file
    pomDir <- Option(pomFile.getParentFile)
    parentDir <- Option(pomDir.getParentFile)
    parentFile = new File(parentDir, "pom.xml")
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
