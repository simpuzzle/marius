
scalaVersion := "2.11.7"

val simpuzzle = "2.0-SNAPSHOT"

libraryDependencies += "fr.iscpif.simpuzzle" %% "puzzle" % simpuzzle
libraryDependencies += "fr.iscpif.simpuzzle" %% "gis" % simpuzzle

resolvers += Resolver.sonatypeRepo("snapshots") 

