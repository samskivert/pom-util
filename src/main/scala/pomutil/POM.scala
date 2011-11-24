//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import java.io.File
import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer
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
    (elem \ "dependencies" \ "dependency") map(Dependency.fromXML(_pfunc))
  lazy val modules :Seq[String] =
    (elem \ "modules" \\ "module") map(_.text.trim)

  /** Returns an identifier that encompases the group, artifact and version. */
  def id = groupId + ":" + artifactId + ":" + version

  // TODO: other bits

  /** Looks up a POM attribute, which may include properties defined in the POM as well as basic
   * project attributes like `project.version`, etc. */
  def getAttr (name :String) :Option[String] =
    // TODO: support env.x and Java system properties?
    getProjectAttr(name) orElse properties.get(name) orElse parent.flatMap(_.properties.get(name))

  /** Returns a dependency on the (optionally classified) artifact described by this POM. */
  def toDependency (classifier :Option[String] = None,
                    scope :String = Dependency.DefaultScope,
                    optional :Boolean = false) =
    Dependency(groupId, artifactId, version, packaging, classifier, scope, optional)

  /** Returns true if this POM declares a snapshot artifact, false otherwise. */
  def isSnapshot = version.endsWith("-SNAPSHOT")

  override def toString = groupId + ":" + artifactId + ":" + version +
    parent.map(p => "\n  (p: " + p + ")").getOrElse("")

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

  private def attr (name :String) = XMLUtil.text(elem, name) map(_pfunc)

  private lazy val _pfunc = (text :String) => {
    val m = PropRe.matcher(text)
    val sb = new StringBuffer
    while (m.find()) {
      val name = m.group(1)
      m.appendReplacement(sb, getAttr(name).getOrElse("\\$!{" + name + "}"))
    }
    m.appendTail(sb)
    sb.toString
  }
}

object POM {
  import XMLUtil._

  /** Parses the POM in the specified file. */
  def fromFile (file :File) :Option[POM] = fromXML(XML.loadFile(file), Some(file.getAbsoluteFile))

  /** Parses a POM from the supplied XML. */
  def fromXML (node :Node, file :Option[File]) :Option[POM] = node match {
    case elem if (elem.label == "project") => {
      val parentDep = (elem \ "parent").headOption map(Dependency.fromXML) map(_.copy(`type` = "pom"))
      val parent = try {
        localParent(file, parentDep) orElse installedParent(parentDep)
      } catch {
        case e => println("Failed to read parent pom (" + parentDep + "): " + e.getMessage) ; None
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
    val parentFile = new File(parentDir, "pom.xml")
    if (parentFile.exists)
    pom <- fromFile(parentFile)
    if (pom.toDependency() == pdep)
  } yield pom

  private def installedParent (parentDep :Option[Dependency]) = for {
    pdep <- parentDep
    pfile <- pdep.localPOM
    pom <- fromFile(pfile)
  } yield pom

  private val PropRe = Pattern.compile("\\$\\{([^}]+)\\}")
}
