//
// $Id$

package pomutil

import java.io.{File, FileReader, Reader}
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
  def fromFile (file :File) :Option[POM] = fromReader(new FileReader(file))

  /** Parses the POM via the specified reader. */
  def fromReader (in :Reader) :Option[POM] = fromXML(XML.load(in))

  /** Parses a POM from the supplied XML. */
  def fromXML (node :Node) :Option[POM] = node match {
    case elem if (elem.label == "project") =>
      val errors = ArrayBuffer[String]()
      val parent = for {
        pdep <- (elem \ "parent").headOption
        pfile <- Dependency.fromXML(pdep).localPOM
        pom <- try fromFile(pfile) catch {
          case e =>
            errors += "Failed to read parent pom (" + pfile + "): " + e.getMessage
          None
        }
      } yield pom

      Some(POM(
        parent,
        text(elem, "modelVersion") getOrElse("4.0.0"),
        text(elem, "groupId") getOrElse(parent map(_.groupId) getOrElse("missing")),
        text(elem, "artifactId") getOrElse("missing"),
        text(elem, "version") getOrElse(parent map(_.version) getOrElse("missing")),
        text(elem, "packaging") getOrElse(parent map(_.packaging) getOrElse("missing")),
        text(elem, "name"),
        text(elem, "description"),
        text(elem, "url"),
        (elem \ "dependencies" \ "dependency") map(Dependency.fromXML),
        errors.toSeq))
    case _ => None
  }
}
