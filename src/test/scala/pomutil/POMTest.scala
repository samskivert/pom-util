//
// $Id$

package pomutil

import org.junit.Test
import org.junit.Assert._

/**
 * Tests for the `POM` code.
 */
class POMTest
{
  import POM._

  val samskivert =
    <project>
      <modelVersion>4.0.0</modelVersion>
      <parent>
        <groupId>org.sonatype.oss</groupId>
        <artifactId>oss-parent</artifactId>
        <version>7</version>
      </parent>

      <groupId>com.samskivert</groupId>
      <artifactId>samskivert</artifactId>
      <version>1.6-SNAPSHOT</version>
      <packaging>jar</packaging>

      <name>samskivert</name>
      <url>http://github.com/samskivert/samskivert</url>
    </project>

  val mavenCore =
    <dependency>
      <groupId>org.apache.maven</groupId>
      <artifactId>maven-core</artifactId>
      <version>2.2.1</version>
    </dependency>

  @Test def testFromXML () {
    val pom = fromXML(samskivert).get
    assertEquals(Some(Dependency("org.sonatype.oss", "oss-parent", "7", "pom")),
                 pom.parent.map(_.toDependency()))
    assertEquals("com.samskivert", pom.groupId)
    assertEquals("samskivert", pom.artifactId)
    assertEquals("1.6-SNAPSHOT", pom.version)
    assertEquals("jar", pom.packaging)
    assertEquals(Some("samskivert"), pom.name)
    assertEquals(None, pom.description)
    assertEquals(Some("http://github.com/samskivert/samskivert"), pom.url)
  }

  @Test def testFromFile () {
    val pom = Dependency.fromXML(mavenCore).localPOM.flatMap(f => fromFile(f)).get
    assertEquals(Some(Dependency(pom.groupId, "maven", pom.version, "pom")),
                 pom.parent.map(_.toDependency()))
    assertEquals("org.apache.maven", pom.groupId)
    assertEquals("maven-core", pom.artifactId)
    assertEquals("2.2.1", pom.version)
    assertEquals("pom", pom.packaging)
    assertEquals(Some("Maven Core"), pom.name)
    assertEquals(None, pom.description)
    assertEquals(None, pom.url)
  }
}
