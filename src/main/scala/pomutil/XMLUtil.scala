//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import java.util.regex.Pattern

import scala.xml.Node

/**
 * Some XML utilities.
 */
object XMLUtil {
  /** Obtains the text of the (first instance of the) specified child of the supplied node, if it
   * exists. Trims whitespace along the way. */
  def text (node :Node, child :String) :Option[String] =
    (node \ child).headOption map(_.text.trim)

  /** Creates a function that takes text and substitutes any properties in the text for their
   * values in the supplied map. */
  def mkPropFunc (props :Map[String,String]) = (text :String) => {
    val m = PropRe.matcher(text)
    val sb = new StringBuffer
    while (m.find()) {
      val name = m.group(1)
      m.appendReplacement(sb, props.getOrElse(name, "\\$!{" + name + "}"))
    }
    m.appendTail(sb)
    sb.toString
  }

  private val PropRe = Pattern.compile("\\$\\{([^}]+)\\}")
}
