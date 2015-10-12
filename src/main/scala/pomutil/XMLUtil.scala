//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import scala.xml.NodeSeq

/**
 * Some XML utilities.
 */
object XMLUtil {

  /** Returns the inner text of all elements in `node`. Generally this will be passed a node seq
    * that contains the inside of one of Maven's standard "say what it is, then repeat what it is
    * for every child node". For example if you had `modes` equal to the node:
    * ```
    * <modules>
    *   <module>foo</module>
    *   <module>bar</module>
    *   <module>baz</module>
    *   </modules>
    * ```
    * then you would call `text(mods \ "modules")` to get back `Seq("foo", "bar", "baz")`.
    */
  def text (node :NodeSeq) :Seq[String] = node map(_.text.trim)

  /** Obtains the text of the (first instance of the) specified child of the supplied node, if it
    * exists. Trims whitespace along the way. */
  def text (node :NodeSeq, child :String) :Option[String] = text(node \ child).headOption
}
