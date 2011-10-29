//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import org.junit.Test
import org.junit.Assert._

/**
 * Tests for the `Dependency` code.
 */
class DependencyTest
{
  import Dependency._

  val mavenCore =
    <dependency>
      <groupId>org.apache.maven</groupId>
      <artifactId>maven-core</artifactId>
      <version>2.2.1</version>
    </dependency>

  val mavenCompiler =
    <dependency>
      <groupId>org.apache.maven.plugins</groupId>
      <artifactId>maven-compiler-plugin</artifactId>
      <version>2.3</version>
    </dependency>

  val full =
    <dependency>
      <groupId>com.threerings</groupId>
      <artifactId>aspirin</artifactId>
      <version>1.0</version>
      <type>swf</type>
      <classifier>sources</classifier>
      <scope>test</scope>
      <optional>true</optional>
    </dependency>

  @Test def testFromXML () {
    assertEquals(Dependency("org.apache.maven", "maven-core", "2.2.1",
                            DefaultType, None, DefaultScope, false), fromXML(mavenCore))
    assertEquals(Dependency("com.threerings", "aspirin", "1.0",
                            "swf", Some("sources"), "test", true), fromXML(full))
  }

  @Test def testLocalFiles () {
    assertTrue(fromXML(mavenCore).localPOM.isDefined)
    assertFalse(fromXML(full).localPOM.isDefined)
    assertTrue(fromXML(mavenCompiler).localArtifact.isDefined)
    assertFalse(fromXML(full).localArtifact.isDefined)
  }

  @Test def testArtifactName () {
    assertEquals("maven-compiler-plugin-2.3.jar", fromXML(mavenCompiler).artifactName)
    assertEquals("aspirin-1.0-sources.swf", fromXML(full).artifactName)
  }
}
