
import java.io.File
import sbt._
import Process._
import Keys._

organization := "de.deterministic-arts"

name := "DartsLibIO"

version := "0.1"

scalaVersion := "2.11.1"

scalaSource in Compile <<= baseDirectory (_ / "src")

