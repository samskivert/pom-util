//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

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
  `type` :String = Dependency.DefaultType,
  classifier :Option[String] = None,
  scope :String = Dependency.DefaultScope,
  optional :Boolean = false
) {
  /** Returns an identifier that encompases the group, artifact and version. */
  def id = groupId + ":" + artifactId + ":" + version

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

  /** Parses a dependency from the supplied XML, using the supplied property substitution function
   * to handle property replacement. */
  def fromXML (pfunc :(String => String))(node :Node) :Dependency = Dependency(
    text(node, "groupId") map(pfunc) getOrElse("missing"),
    text(node, "artifactId") map(pfunc) getOrElse("missing"),
    text(node, "version") map(pfunc) getOrElse("missing"),
    text(node, "type") map(pfunc) getOrElse(DefaultType),
    text(node, "classifier") map(pfunc),
    text(node, "scope") map(pfunc) getOrElse(DefaultScope),
    text(node, "optional") map(pfunc) map(_.equalsIgnoreCase("true")) getOrElse(false))

  /** Parses a dependency from the supplied XML, doing no property substitution. */
  def fromXML (node :Node) :Dependency = fromXML(ident)(node)

  private def optRepoFile (segs :String*) = fileToOpt(file(m2repo, segs :_*))
  private def fileToOpt (file :File) = if (file.exists) Some(file) else None
  private def file (root :File, segs :String*) = (root /: segs)(new File(_, _))

  private val m2repo = file(new File(System.getProperty("user.home")), ".m2", "repository")
  private val ident = (text :String) => text
}
