# pom-util

This is a Scala library for extracting project metadata from Maven POM files.
It does not aim to reproduce all of Maven's functionality, but rather to make
it easy to extract basic metadata for a project for use by simple development
tools.

## Usage

The [POM] class exposes the metadata from a `pom.xml` file:

    val pom = POM.fromFile(new File("pom.xml"))
    // do what you will with pom.groupId, pom.artifactId, etc.

A POM's dependencies are modeled via the [Dependency] class, and one can locate
said dependencies in the local Maven repository if desired:

    val pom = POM.fromFile(new File("pom.xml"))
    pom.depends map(_.localArtifact) foreach { f => println("File: " + f.getPath) }

## License

pom-util is released under the New BSD License, which can be found in the
[LICENSE] file.

[POM]: https://github.com/samskivert/pom-util/blob/master/src/main/scala/pomutil/POM.scala
[Dependency]: https://github.com/samskivert/pom-util/blob/master/src/main/scala/pomutil/Dependency.scala
[LICENSE]: https://github.com/samskivert/pom-util/blob/master/LICENSE
