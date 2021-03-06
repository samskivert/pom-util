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

      <scm>
        <connection>scm:git:git://github.com/samskivert/samskivert.git</connection>
        <developerConnection>scm:git:git@github.com:samskivert/samskivert.git</developerConnection>
        <url>http://github.com/samskivert/samskivert/</url>
      </scm>

      <properties>
        <foo>bar</foo>
        <servlet.version>2.5</servlet.version>
        <recurse.version>${{servlet.version}}</recurse.version>
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

  val metaPom =
    <project>
      <groupId>com.samskivert</groupId>
      <artifactId>metaproject</artifactId>
      <version>1.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>sub1</module>
        <module>sub2</module>
        <module>sub3</module>
      </modules>
    </project>

  val profiled =
    <project>
      <groupId>com.test</groupId>
      <artifactId>profiled</artifactId>
      <version>1.0-SNAPSHOT</version>
      <packaging>jar</packaging>

      <modules>
        <module>core</module>
      </modules>

      <profiles>
        <profile>
          <id>java</id>
          <modules><module>java</module></modules>
        </profile>
        <profile>
          <id>android</id>
          <modules><module>android</module></modules>
        </profile>
      </profiles>
    </project>

  val buildPropped =
    <project>
      <groupId>com.test</groupId>
      <artifactId>build-propped</artifactId>
      <version>1.0-SNAPSHOT</version>
      <packaging>jar</packaging>

      <build>
        <sourceDirectory>src</sourceDirectory>
        <testSourceDirectory>tests</testSourceDirectory>
        <plugins>
          <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-source-plugin</artifactId>
            <configuration>
              <foo>bar</foo>
              <bars><bar>baz</bar><bar>bing</bar></bars>
            </configuration>
            <executions>
              <execution>
                <id>attach-sources</id>
                <phase>generate-resources</phase>
                <goals>
                  <goal>jar-no-fork</goal>
                </goals>
              </execution>
            </executions>
         </plugin>
        </plugins>

        <resources>
          <resource>
            <directory>rsrcs</directory>
            <includes><include>foo*</include></includes>
            <excludes><exclude>bar*</exclude></excludes>
          </resource>
        </resources>

        <testResources>
          <testResource>
            <directory>tests</directory>
          </testResource>
        </testResources>
      </build>
    </project>

  val envSysPath =
    <project>
      <groupId>com.test</groupId>
      <artifactId>build-propped</artifactId>
      <version>1.0-SNAPSHOT</version>
      <packaging>jar</packaging>
      <dependencies>
        <dependency>
          <groupId>com.google.android</groupId>
          <artifactId>android</artifactId>
          <version>23</version>
          <scope>system</scope>
          <systemPath>{"${env.HOME}/foo.jar"}</systemPath>
        </dependency>
      </dependencies>
    </project>

  @Test def testEnvSysPath () {
    val pom = fromXML(envSysPath, None).get
    val foodep = pom.depends.head
    val foopath = foodep.systemPath.get
    assertFalse(foopath contains System.getProperty("user.dir"))
  }

  @Test def testFromXML () {
    val pom = fromXML(samskivert, None).get
    assertEquals(Some(Dependency("org.sonatype.oss", "oss-parent", "7", "pom")),
                 pom.parent.map(_.toDependency()))
    assertEquals("com.samskivert", pom.groupId)
    assertEquals("samskivert", pom.artifactId)
    assertEquals("1.6-SNAPSHOT", pom.version)
    assertEquals("jar", pom.packaging)
    assertEquals(SCM(Some("scm:git:git://github.com/samskivert/samskivert.git"),
                     Some("scm:git:git@github.com:samskivert/samskivert.git"),
                     Some("http://github.com/samskivert/samskivert/")), pom.scm)
    assertEquals(Some("samskivert"), pom.name)
    assertEquals(None, pom.description)
    assertEquals(Some("http://github.com/samskivert/samskivert"), pom.url)
    assertEquals(Map("foo" -> "bar", "servlet.version" -> "2.5",
                     "recurse.version" -> "${servlet.version}"), pom.properties)
    assertEquals(Seq(Dependency("javax.servlet", "servlet-api", "2.5", scope="provided"),
                     Dependency("log4j", "log4j", "1.2.16", optional=true)), pom.depends)
    // check that we can read a property defined in our parent POM
    assertEquals(Some("https://oss.sonatype.org/content/repositories/snapshots/"),
                 pom.getAttr("sonatypeOssDistMgmtSnapshotsUrl"))

    // check that properties which reference other properties also work
    assertEquals(pom.getAttr("servlet.version"), pom.getAttr("recurse.version"))

    // test modules parsing
    val mpom = fromXML(metaPom, None).get
    assertEquals(Seq("sub1", "sub2", "sub3"), mpom.modules)
  }

  @Test def testFromFile () {
    val pom = Dependency.fromXML(mavenCore).localPOM.flatMap(fromFile).get
    assertEquals(Some(Dependency(pom.groupId, "maven", pom.version, "pom")),
                 pom.parent.map(_.toDependency()))
    assertEquals("org.apache.maven", pom.groupId)
    assertEquals("maven-core", pom.artifactId)
    assertEquals("2.2.1", pom.version)
    assertEquals("jar", pom.packaging)
    assertEquals(Some("Maven Core"), pom.name)
    assertEquals(None, pom.description)
    assertEquals(None, pom.url)
    assertEquals(SCM(None, None, None), pom.scm)
  }

  @Test def testBogusRelativePath () {
    val spiceParent = Dependency("org.sonatype.spice", "spice-parent", "17", "pom")
    val pom = spiceParent.localPOM.flatMap(fromFile) match {
      case Some(pom) => assertTrue("spice-parent resolvable", pom.parent.isDefined)
      case None => // no spice, skip it
    }
  }

  @Test def testProfiles () {
    val pom = fromXML(profiled, None).get
    assertEquals(pom.profiles.map(_.id), Seq("java", "android"))
    assertEquals(pom.profiles.map(_.modules), Seq(Seq("java"), Seq("android")))
    assertEquals(pom.allModules, Seq("core", "java", "android"))
    assertEquals(pom.modules("java"), Seq("core", "java"))
    assertEquals(pom.modules("android"), Seq("core", "android"))
  }

  @Test def testBuildProps () {
    val pom = fromXML(buildPropped, None).get
    assertEquals("src", pom.buildProps("sourceDirectory"))
    assertEquals("tests", pom.buildProps("testSourceDirectory"))
    assertEquals(Seq(Resource("rsrcs", false, Seq("foo*"), Seq("bar*"))), pom.resources)
    assertEquals(Seq(Resource("tests")), pom.testResources)
  }

  @Test def testPlugins () {
    val pom = fromXML(buildPropped, None).get
    assertEquals(1, pom.plugins.size)
    assertEquals(Some("bar"), pom.plugins.head.configValue("foo"))
    assertEquals(Seq("baz", "bing"), pom.plugins.head.configList("bars", "bar"))
    assertEquals(None, pom.plugins.head.configValue("potato"))
    assertEquals(Seq(), pom.plugins.head.configList("bars", "bangs"))
    assertEquals(Seq(), pom.plugins.head.configList("bingle", "bangle"))
  }

  @Test def testTransDeps () {
    def ids (d :Dependency) = d.id + ":" + d.scope

    val samdep = Dependency("com.samskivert", "samskivert", "1.6")
    samdep.localPOM.flatMap(POM.fromFile) foreach { pom =>
      // new DependResolver(pom).resolve(false).map(ids) foreach println
      assertEquals(Seq("javax.servlet:servlet-api:2.5:provided",
                       "log4j:log4j:1.2.16:compile",
                       "javax.mail:mail:1.4.1:compile",
                       "org.apache.velocity:velocity:1.6.4:compile",
                       "commons-digester:commons-digester:2.0:compile",
                       "javax.activation:activation:1.1:compile",
                       "commons-collections:commons-collections:3.2.1:compile",
                       "commons-lang:commons-lang:2.4:compile",
                       "oro:oro:2.0.8:compile",
                       "commons-beanutils:commons-beanutils:1.8.0:compile",
                       "commons-logging:commons-logging:1.1.1:compile"),
                   new DependResolver(pom).resolve().map(ids))
      // new DepenResolver(pom).resolve(true).map(ids) foreach println
      assertEquals(Seq("javax.servlet:servlet-api:2.5:provided",
                       "log4j:log4j:1.2.16:compile",
                       "javax.mail:mail:1.4.1:compile",
                       "org.apache.velocity:velocity:1.6.4:compile",
                       "commons-digester:commons-digester:2.0:compile",
                       "javax.activation:activation:1.1:compile",
                       "commons-collections:commons-collections:3.2.1:compile",
                       "commons-lang:commons-lang:2.4:compile",
                       "oro:oro:2.0.8:compile",
                       "commons-beanutils:commons-beanutils:1.8.0:compile",
                       "commons-logging:commons-logging:1.1.1:compile",
                       "junit:junit:4.10:test",
                       "org.hsqldb:hsqldb:2.2.4:test",
                       "org.hamcrest:hamcrest-core:1.1:test"),
                   new DependResolver(pom).resolve(DependResolver.Test).map(ids))
    }

    val jettydep = Dependency("org.eclipse.jetty", "jetty-servlet", "9.0.0.RC2")
    jettydep.localPOM.flatMap(POM.fromFile) foreach { pom =>
      // new DependResolver(pom).resolve(false).map(ids) foreach println
      assertEquals(Seq("org.eclipse.jetty:jetty-security:9.0.0.RC2:compile",
                       "org.eclipse.jetty:jetty-jmx:9.0.0.RC2:compile",
                       "org.eclipse.jetty:jetty-server:9.0.0.RC2:compile",
                       "org.eclipse.jetty.orbit:javax.servlet:3.0.0.v201112011016:compile",
                       "org.eclipse.jetty:jetty-http:9.0.0.RC2:compile",
                       "org.eclipse.jetty:jetty-io:9.0.0.RC2:compile",
                       // this depend comes in twice from two parents, but should only appear once
                       "org.eclipse.jetty:jetty-util:9.0.0.RC2:compile"),
                   new DependResolver(pom).resolve().map(ids))
    }
  }
}
