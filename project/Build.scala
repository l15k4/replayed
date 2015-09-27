import com.lihaoyi.workbench.Plugin._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt.Keys._
import sbt._

object Build extends sbt.Build {

  lazy val replayed =
    project.in(file("."))
      .enablePlugins(ScalaJSPlugin)
      .settings(workbenchSettings:_*)
      .settings(
        organization := "com.viagraphs",
        name := "replayed",
        version := "0.2",
        scalaVersion := "2.11.7",
        resolvers += Resolver.mavenLocal,
        watchSources += baseDirectory.value / "index.html",
        scalacOptions ++= Seq(
          "-unchecked", "-deprecation", "-feature",
          "-Xlint", "-Xfuture", "-Xfatal-warnings",
          "-Yinline-warnings", "-Ywarn-adapted-args", "-Ywarn-inaccessible",
          "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Yno-adapted-args"
        ),
        libraryDependencies ++= Seq(
          "org.scala-js" %%% "scalajs-dom" % "0.8.1",
          "com.viagraphs" %%% "scalajs-keyboard-polyfill" % "0.0.4-SNAPSHOT",
          "com.viagraphs" %%% "scalajs-rx-idb" % "0.0.7",
          "com.viagraphs" %%% "actuarius" % "0.3.0-SNAPSHOT",
          "org.monifu" %%% "monifu" % "1.0-RC3",
          "com.lihaoyi" %%% "scalatags" % "0.5.2",
          "com.lihaoyi" %%% "upickle" % "0.3.6",
          "com.lihaoyi" %%% "utest" % "0.3.1" % "test"
        ),
        bootSnippet := "com.viagraphs.replayed.Replayed().main();",
        scalaJSStage := FastOptStage,
        testFrameworks += new TestFramework("utest.runner.Framework"),
        autoAPIMappings := true,
        updateBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
        emitSourceMaps in fullOptJS := false,
        relativeSourceMaps := true,
        requiresDOM := true,
        persistLauncher := true,
        persistLauncher in Test := false
      )
}
