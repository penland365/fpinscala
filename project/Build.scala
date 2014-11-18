import sbt._
import Keys._

object FPInScala extends Build {

   scalacOptions ++= Seq("-encoding", "utf8", "-unchecked", "-deprecation")

   lazy val root = Project(id = "root", base = file(".")) aggregate(gettingstarted)
   lazy val gettingstarted = Project(id = "gettingstarted", base = file("gettingstarted"))
   lazy val datastructures = Project(id = "datastructures", base = file("datastructures"))
   lazy val errorhandling = Project(id = "errorhandling", base = file("errorhandling"))
}
