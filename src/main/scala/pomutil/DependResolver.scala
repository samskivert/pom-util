//
// pom-util - a Scala library for reading Maven POM files
// http://github.com/samskivert/pom-util/blob/master/LICENSE

package pomutil

import scala.collection.mutable.{ArrayBuffer, Set => MSet}

/** Resolves transitive dependencies for `pom`.
  *
  * If this POM is part of a multi-module project, sibling dependencies will be resolved via the
  * POMs in sibling directories rather than via the .m2 repository. This differs from Maven, but
  * is vastly more useful and I wish Maven did things this way.
  *
  * A mechanism is also provided for locating SNAPSHOT dependencies via a "workspace" mechanism
  * rather than relying on `~/.m2`. This allows one to incorporate up-to-date project data from
  * "working copies" of SNAPSHOT projects rather than the POM most recently installed into `~/.m2`.
  */
class DependResolver (pom :POM) {

  // if we're part of a multimodule project, we want to resolve our "sibling" dependencies from
  // their neighboring directories rather than looking for them in the .m2 repository
  private val sibDeps :Map[String,POM] = pom.rootPOM map(POM.allModules) getOrElse(Map())

  /** Resolves our POM's transitive dependencies. Exclusions are honored, and conflicts are resolved
    * using the standard "distance from root POM" Maven semantics. Direct dependencies with scopes
    * other than `compile` and `test` are included for this project, but not transitively, also per
    * standard Maven semantics.
    *
    * @param forTest whether to include test dependencies.
    */
  def resolve (forTest :Boolean) :Seq[Dependency] = {
    val haveDeps = MSet[(String,String)]()
    val allDeps = ArrayBuffer[Dependency]()
    def key (d :Dependency) = (d.groupId, d.artifactId)

    // we expand our dependency tree one "layer" at a time; we start with the depends at distance
    // one from the project (its direct dependencies), then we compute all of the direct
    // dependencies of those depends (distance two) and filter out any that are already satisfied
    // (this enforces the "closest to the root POM" conflict resolution strategy), then we repeat
    // the process, expanding to distance three and so forth, until we discover no new depends

    def extract (deps :Seq[Dependency], mapper :(Dependency => Dependency)) {
      haveDeps ++= (deps map key)
      allDeps ++= deps
      val newdeps = for {
        dep <- deps
        // if we have a local version of depend, use it, otherwise get it from ~/.m2/repository
        pom <- (localDep(dep) orElse dep.localPOM.flatMap(POM.fromFile)).toSeq
        dd <- pom.depends filterNot(d => dep.exclusions((d.groupId, d.artifactId)))
        if (dd.scope == "compile" && !dd.optional && !haveDeps(key(dd)))
      } yield mapper(dd)
      // we might encounter the same dep from two parents, so we .distinct to consolidate
      if (!newdeps.isEmpty) extract(newdeps.distinct, mapper)
    }

    extract(rootDepends(false), d => d)
    if (forTest) extract(rootDepends(true), _.copy(scope="test"))

    allDeps.toSeq
  }

  /** Returns the root depends from which the transitive depends are expanded. By default this is all
    * the depends in `pom`, but derived classes may wish to customize.
    */
  protected def rootDepends (forTest :Boolean) :Seq[Dependency] =
    pom.depends filter(d => (d.scope == "test") == forTest)

  /** Checks for a "local" version of `dep`. The default implementation checks whether `dep`
    * represents a sibling module in a mutli-module project, and returns the working copy of the
    * sibling POM.
    *
    * Projects that know of other local depends can override this method and return them here.
    * For example an IDE which has the latest snapshot version of a dependency checked out into
    * its workspace may wish to use that working copy when resolving dependencies for projects
    * that depend on it, instead of using whatever was most recently installed into `~/.m2`.
    */
  protected def localDep (dep :Dependency) :Option[POM] = sibDeps.get(dep.id)
}
