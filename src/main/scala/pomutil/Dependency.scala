//
// $Id$

package pomutil

import java.io.File
import scala.xml.Node

/**
 * Project dependency metadata.
 */
case class Dependency (
  groupId :String,
  artifactId :String,
  version :String,
  `type` :String,
  classifier :Option[String],
  scope :String,
  optional :Boolean
) {
  /** Returns the path to this artifact, relative to the Maven repo root. */
  def repositoryPath :Seq[String] = groupId.split("\\.") ++ Seq(artifactId, version)

  /** Returns the name of the artifact described by this dependency. */
  def artifactName :String =
    artifactId + "-" + version + classifier.map("-" + _).getOrElse("") + "." + `type`

  /** Returns the name of the POM that describes this dependency. */
  def pomName = artifactId + "-" + version + ".pom"

  /** Returns true if this dependency references a snapshot artifact, false otherwise. */
  def isSnapshot = version.endsWith("-SNAPSHOT")

  /** Locates the specified dependency in the user's local Maven repository and returns the POM
   * file, if it exists. */
  def localPOM :Option[File] = Dependency.optRepoFile(repositoryPath :+ pomName :_*)

  /** Locates the specified dependency in the user's local Maven repository and returns the
   * artifact file, if it exists. */
  def localArtifact :Option[File] = Dependency.optRepoFile(repositoryPath :+ artifactName :_*)
}

/**
 * `Dependency`-related utilities.
 */
object Dependency {
  import XMLUtil._

  /** The default `type` for a dependency: `jar`. */
  val DefaultType = "jar"

  /** The default `scope` for a dependency: `compile`. */
  val DefaultScope = "compile"

  /** Creates a dependency, using defaults where desired. */
  def apply (groupId :String, artifactId :String, version :String, `type` :String = DefaultType,
             classifier :Option[String] = None, scope :String = DefaultScope) :Dependency =
    new Dependency(groupId, artifactId, version, `type`, classifier, scope, false)

  /** Parses a dependency from the supplied XML. */
  def fromXML (node :Node) = Dependency(
    text(node, "groupId") getOrElse("missing"),
    text(node, "artifactId") getOrElse("missing"),
    text(node, "version") getOrElse("missing"),
    text(node, "type") getOrElse(DefaultType),
    text(node, "classifier"),
    text(node, "scope") getOrElse(DefaultScope),
    text(node, "optional") map(_.equalsIgnoreCase("true")) getOrElse(false))

  private def optRepoFile (segs :String*) = fileToOpt(file(m2repo, segs :_*))

  private def fileToOpt (file :File) = if (file.exists) Some(file) else None

  private def file (root :File, segs :String*) = (root /: segs) ((f, s) => new File(f, s))

  private val home = new File(System.getProperty("user.home"))
  private val m2 = new File(home, ".m2")
  private val m2repo = new File(m2, "repository")
}
