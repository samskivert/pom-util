//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import scala.xml.Node

/** Modules profile information inside a POM. */
class Profile (pom :POM, elem :Node) {
  /** The id of this profile. */
  lazy val id :String = pom.attr(elem, "id") getOrElse("")

  /** The dependencies declared in this profile. */
  lazy val depends :Seq[Dependency] =
    (elem \ "dependencies" \ "dependency") map(Dependency.fromXML(pom.subProps))

  /** The modules declared in this profile. */
  lazy val modules :Seq[String] =
    (elem \ "modules" \\ "module") map(_.text.trim)

  override def toString = id
}
