//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import scala.xml.Node

/**
 * Some XML utilities.
 */
object XMLUtil {
  /** Obtains the text of the (first instance of the) specified child of the supplied node, if it
   * exists. Trims whitespace along the way. */
  def text (node :Node, child :String) :Option[String] = (node \ child).headOption map(_.text.trim)
}
