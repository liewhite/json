sonatypeProfileName := "io.github.liewhite"

// To sync with Maven central, you need to supply the following information:
publishMavenStyle := true

// Open-source license of your choice
ThisBuild/licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

// Where is the source code hosted: GitHub or GitLab?
import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("liewhite", "common", "leeliewhite@gmail.com"))

// or if you want to set these fields manually
ThisBuild/homepage := Some(url("https://www.github.com/liewhite/common"))
ThisBuild/scmInfo := Some(
  ScmInfo(
    url("https://github.com/liewhite/common"),
    "scm:git@github.com:liewhite/common.git"
  )
)
ThisBuild/developers := List(
  Developer(id="liewhite", name="lilin", email="leeliewhite@gmail.com", url=url("https://liewhite.github.io"))
)