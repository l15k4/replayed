import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt.Keys._
import sbt._
import com.lihaoyi.workbench.Plugin._

object Build extends sbt.Build {

  lazy val replayed =
    project.in(file("."))
      .enablePlugins(ScalaJSPlugin)
      .settings(workbenchSettings:_*)
      .settings(
        organization := "com.viagraphs",
        name := "replayed",
        version := "0.1",
        scalaVersion := "2.11.4",
        resolvers += Resolver.mavenLocal,
        watchSources += baseDirectory.value / "index.html",
        scalacOptions ++= Seq(
          "-unchecked", "-deprecation", "-feature",
          "-Xlint", "-Xfuture", "-Xfatal-warnings",
          "-Yinline-warnings", "-Ywarn-adapted-args", "-Ywarn-inaccessible",
          "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Yno-adapted-args"
        ),
        libraryDependencies ++= Seq(
          "eu.henkelmann" %%% "actuarius" % "0.2.9-SNAPSHOT",
          "org.scala-js" %%% "scalajs-dom" % "0.7.1-SNAPSHOT",
          "com.scalatags" %%% "scalatags" % "0.4.3-M3",
          "org.monifu" %%% "monifu" % "0.1-SNAPSHOT",
          "com.viagraphs" %%% "scalajs-keyboard-polyfill" % "0.0.3-SNAPSHOT",
          "com.viagraphs" %%% "scalajs-rx-idb" % "0.0.6-SNAPSHOT",
          "com.lihaoyi" %%% "upickle" % "0.2.6-M3",
          "com.lihaoyi" %%% "utest" % "0.2.5-M3-SNAPSHOT" % "test"
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
