//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

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

      <properties>
        <foo>bar</foo>
        <servlet.version>2.5</servlet.version>
      </properties>

      <dependencies>
        <dependency>
          <groupId>javax.servlet</groupId>
          <artifactId>servlet-api</artifactId>
          <version>${{servlet.version}}</version>
          <scope>provided</scope>
        </dependency>
        <dependency>
          <groupId>log4j</groupId>
          <artifactId>log4j</artifactId>
          <version>1.2.16</version>
          <optional>true</optional>
        </dependency>
      </dependencies>
    </project>

  val mavenCore =
    <dependency>
      <groupId>org.apache.maven</groupId>
      <artifactId>maven-core</artifactId>
      <version>2.2.1</version>
    </dependency>

  @Test def testFromXML () {
    val pom = fromXML(samskivert, None).get
    assertEquals(Some(Dependency("org.sonatype.oss", "oss-parent", "7", "pom")),
                 pom.parent.map(_.toDependency()))
    assertEquals("com.samskivert", pom.groupId)
    assertEquals("samskivert", pom.artifactId)
    assertEquals("1.6-SNAPSHOT", pom.version)
    assertEquals("jar", pom.packaging)
    assertEquals(Some("samskivert"), pom.name)
    assertEquals(None, pom.description)
    assertEquals(Some("http://github.com/samskivert/samskivert"), pom.url)
    assertEquals(Map("foo" -> "bar", "servlet.version" -> "2.5"), pom.properties)
    assertEquals(Seq(Dependency("javax.servlet", "servlet-api", "2.5", scope="provided"),
                     Dependency("log4j", "log4j", "1.2.16", optional=true)), pom.depends)
  }

  @Test def testFromFile () {
    val pom = Dependency.fromXML(mavenCore).localPOM.flatMap(fromFile).get
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
