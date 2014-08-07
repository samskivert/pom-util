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
  import DependResolver._

  // if we're part of a multimodule project, we want to resolve our "sibling" dependencies from
  // their neighboring directories rather than looking for them in the .m2 repository
  private val sibDeps :Map[String,POM] = pom.rootPOM map(POM.allModules) getOrElse(Map())

  /** Resolves our POM's transitive dependencies. Exclusions are honored, and conflicts are resolved
    * using the standard "distance from root POM" Maven semantics. Direct and transitive
    * dependencies are resolved per the specified scope.
    */
  def resolve (scope :Scope = Compile) :Seq[Dependency] = {
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
      val newDeps = for {
        dep <- deps
        // if we have a local version of depend, use it, otherwise get it from ~/.m2/repository
        pom <- (localDep(dep) orElse dep.localPOM.flatMap(POM.fromFile)).toSeq
        dd <- pom.depends filterNot(d => dep.exclusions((d.groupId, d.artifactId)))
        if (scope.includeTrans(dd.scope) && !dd.optional && !haveDeps(key(dd)))
      } yield mapper(dd)
      if (!newDeps.isEmpty) {
        // we might encounter the same dep from two parents; filter out duplicates after the first
        val seen = MSet[(String,String)]()
        extract(newDeps.filter(d => seen.add(key(d))), mapper)
      }
    }

    extract(rootDepends(Compile), d => d)
    if (scope != Compile) {
      val scopeId = scope.id
      extract(rootDepends(scope), _.copy(scope=scopeId))
    }

    allDeps.toSeq
  }

  @deprecated("Use new resolve(Scope)")
  def resolve (forTest :Boolean) :Seq[Dependency] = resolve(if (forTest) Compile else Test)

  /** Returns the root depends from which the transitive depends are expanded. By default this is
    * all the depends in `pom` (and any depends provided by its parents), but derived classes may
    * wish to customize.
    */
  protected def rootDepends (scope :Scope) :Seq[Dependency] =
    pom.fullDepends filter(d => scope.includeRoot(d.scope))

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

object DependResolver {

  /** Defines dependency inclusion policy for a Maven scope. */
  sealed trait Scope {
    def id :String
    def includeRoot (scope :String) :Boolean
    def includeTrans (scope :String) :Boolean
  }

  /** Includes all non-test, non-runtime root depends, and compile transitive depends. */
  case object Compile extends Scope {
    def id :String = "compile"
    def includeRoot (scope :String) = scope != "test" && scope != "runtime"
    def includeTrans (scope :String) = scope == "compile"
  }

  /** Includes all test or runtime root depends, and compile and runtime transitive depends. */
  case object Test extends Scope {
    def id :String = "test"
    def includeRoot (scope :String) = scope == "test" || scope == "runtime"
    def includeTrans (scope :String) = scope == "compile" || scope == "runtime"
  }

  /** Includes all runtime root depends, and compile and runtime transitive depends. */
  case object Runtime extends Scope {
    def id :String = "runtime"
    def includeRoot (scope :String) = scope == "runtime"
    def includeTrans (scope :String) = scope == "compile" || scope == "runtime"
  }
}
