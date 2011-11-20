//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.xml.{XML, Node}

/**
 * Project metadata.
 */
case class POM (
  parent :Option[POM],
  modelVersion :String,

  groupId :String,
  artifactId :String,
  version :String,
  packaging :String,

  name :Option[String],
  description :Option[String],
  url :Option[String],

  properties :Map[String,String],

  depends :Seq[Dependency],
  // TODO: other bits

  /** Any errors encountered when parsing this POM. */
  errors :Seq[String]
) {
  /** Returns a dependency on the (optionally classified) artifact described by this POM. */
  def toDependency (classifier :Option[String] = None,
                    scope :String = Dependency.DefaultScope,
                    optional :Boolean = false) =
    Dependency(groupId, artifactId, version, packaging, classifier, scope, optional)

  /** Returns true if this POM declares a snapshot artifact, false otherwise. */
  def isSnapshot = version.endsWith("-SNAPSHOT")

  override def toString = groupId + ":" + artifactId + ":" + version +
    parent.map(p => "\n  (p: " + p + ")").getOrElse("")
}

object POM {
  import XMLUtil._

  /** Parses the POM in the specified file. */
  def fromFile (file :File) :Option[POM] = fromXML(XML.loadFile(file), Some(file.getAbsoluteFile))

  /** Parses a POM from the supplied XML. */
  def fromXML (node :Node, file :Option[File]) :Option[POM] = node match {
    case elem if (elem.label == "project") => {
      val errors = ArrayBuffer[String]()
      val parentDep = (elem \ "parent").headOption map(Dependency.fromXML) map(_.copy(`type` = "pom"))
      val parent = try {
        localParent(file, parentDep) orElse installedParent(parentDep)
      } catch {
        case e => println("Failed to read parent pom (" + parentDep + "): " + e.getMessage) ; None
      }

      val props = (elem \ "properties" \ "_") map(toPropTuple) toMap
      val allProps = parent.map(_.properties ++ props) getOrElse(props)
      val pfunc = mkPropFunc(allProps)

      Some(POM(
        parent,
        text(elem, "modelVersion") map(pfunc) getOrElse("4.0.0"),
        text(elem, "groupId") map(pfunc) getOrElse(parent map(_.groupId) getOrElse("missing")),
        text(elem, "artifactId") map(pfunc) getOrElse("missing"),
        text(elem, "version") map(pfunc) getOrElse(parent map(_.version) getOrElse("missing")),
        text(elem, "packaging") map(pfunc) getOrElse(parent map(_.packaging) getOrElse("missing")),
        text(elem, "name") map(pfunc),
        text(elem, "description") map(pfunc),
        text(elem, "url") map(pfunc),
        props,
        (elem \ "dependencies" \ "dependency") map(Dependency.fromXML(pfunc)),
        errors.toSeq))
    }
    case _ => None
  }

  protected def localParent (file :Option[File], parentDep :Option[Dependency]) = for {
    pdep <- parentDep
    pomFile <- file
    pomDir <- Option(pomFile.getParentFile)
    parentDir <- Option(pomDir.getParentFile)
    val parentFile = new File(parentDir, "pom.xml")
    if (parentFile.exists)
    pom <- fromFile(parentFile)
    if (pom.toDependency() == pdep)
  } yield pom

  protected def installedParent (parentDep :Option[Dependency]) = for {
    pdep <- parentDep
    pfile <- pdep.localPOM
    pom <- fromFile(pfile)
  } yield pom

  protected def toPropTuple (node :Node) = (node.label.trim, node.text.trim)
}
