import sbt.Keys.scalacOptions
import scala.sys.process._
import org.enso.build.BenchTasks._

//////////////////////////////
//// Global Configuration ////
//////////////////////////////

val scalacVersion = "2.12.10"
val graalVersion  = "19.2.0.1"
scalaVersion in ThisBuild := scalacVersion

//////////////////////////
//// Compiler Options ////
//////////////////////////

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",                       // Emit warning and location for usages of deprecated APIs.
  "-encoding",                          // Provide explicit encoding (the next line)
  "utf-8",                              // Specify character encoding used by source files.
  "-explaintypes",                      // Explain type errors in more detail.
  "-feature",                           // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",             // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros",      // Allow macro definition (besides implementation and application)
  "-language:higherKinds",              // Allow higher-kinded types
  "-language:implicitConversions",      // Allow definition of implicit functions called views
  "-unchecked",                         // Enable additional warnings where generated code depends on assumptions.
  "-Xlint:adapted-args",                // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative",   // By-name parameter of right associative operator.
  "-Xlint:constant",                    // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",          // Selecting member of DelayedInit.
  "-Xlint:doc-detached",                // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",                // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                   // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",        // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",            // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",                // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",             // Option.apply used implicit view.
  "-Xlint:package-object-classes",      // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",      // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",              // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                 // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",       // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match",               // Pattern match may not be typesafe.
  "-Yno-adapted-args",                  // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification",              // Enable partial unification in type constructor inference
  "-Ywarn-dead-code",                   // Warn when dead code is identified.
  "-Ywarn-extra-implicit",              // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible",                // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any",                   // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",            // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",                // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",               // Warn when numerics are widened.
  "-Ywarn-unused:implicits",            // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",              // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",               // Warn if a local definition is unused.
  "-Ywarn-unused:params",               // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",              // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",             // Warn if a private member is unused.
  "-Ywarn-value-discard",               // Warn when non-Unit expression results are unused.
  "-Ypartial-unification",              // Enable partial unification (which is enabled by default in Scala 2.13).
  "-Xmacro-settings:-logging@org.enso", // Disable the debug logging globally.
  "-Xcheckinit"                         // Wrap field accessors to throw an exception on uninitialized access.
)

/////////////////////////////////
//// Benchmark Configuration ////
/////////////////////////////////

lazy val Benchmark = config("bench") extend sbt.Test

// Native Image Generation
lazy val buildNativeImage =
  taskKey[Unit]("Build native image for executable")

////////////////////////
//// Global Project ////
////////////////////////

lazy val PseudoParser = (project in file("."))
  .settings(version := "0.1")
  .settings(Global / concurrentRestrictions += Tags.exclusive(Exclusive))
  .dependsOn(logger, flexer, syntax_definition)
  .configs(Test)
  .configs(Benchmark)
  .settings(
    mainClass in (Compile, run) := Some("org.PseudoLang.Main"),
    version := "0.1",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
    inConfig(Benchmark)(Defaults.testSettings),
    bench := (test in Benchmark).tag(Exclusive).value,
    parallelExecution in Benchmark := false,
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.17" % "bench",
      "org.scalatest"     %% "scalatest"  % "3.0.5" % Test,
      "com.lihaoyi"       %% "pprint"     % "0.5.3"
    ),
    (Compile / compile) := (Compile / compile)
      .dependsOn(Def.taskDyn {
        val parserCompile =
          (syntax_definition / Compile / compileIncremental).value
        if (parserCompile.hasModified) {
          Def.task {
            streams.value.log.info("Parser changed, forcing recompilation.")
            clean.value
          }
        } else Def.task {}
      })
      .value
  )

////////////////////////////
//// Dependency Bundles ////
////////////////////////////

val monocle = {
  val monocleVersion = "1.6.0"
  Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-law"   % monocleVersion % "test"
  )
}
val cats = {
  Seq(
    "org.typelevel" %% "cats-core" % "2.0.0-RC1",
    "org.typelevel" %% "kittens"   % "2.0.0"
  )
}

val scala_compiler = Seq(
  "org.scala-lang" % "scala-reflect"  % scalacVersion,
  "org.scala-lang" % "scala-compiler" % scalacVersion
)

val circe = Seq("circe-core", "circe-generic", "circe-yaml")
  .map("io.circe" %% _ % "0.10.0")

//////////////////////
//// Sub-Projects ////
//////////////////////

lazy val logger = (project in file("lib/logger"))
  .dependsOn(unused)
  .settings(
    version := "0.1",
    libraryDependencies ++= scala_compiler
  )

lazy val flexer = (project in file("lib/flexer"))
  .dependsOn(logger)
  .settings(
    version := "0.1",
    scalacOptions -= "-deprecation", // FIXME
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= scala_compiler ++ Seq(
      "org.feijoas" %% "mango" % "0.14"
    )
  )

lazy val unused = (project in file("lib/unused"))
  .settings(version := "0.1", scalacOptions += "-nowarn")

lazy val syntax_definition = (project in file("syntax"))
  .dependsOn(logger, flexer)
  .settings(
    libraryDependencies ++= monocle ++ cats ++ scala_compiler ++ Seq()
  )
