package tools_ // general utilities

import types_._
import language.implicitConversions

def enter( // prompt user for input
  label: Text = "", // label for requested input
  default: Text = "" // default value if nothing entered
  ): Text =

  print(s"enter $label [$default]: ")
  val entry = scala.io.StdIn.readLine()
  if entry == "" then default else entry

def getenvx(name: Text, default: Text) =
  // get environment variable or default if not set
  val x = System.getenv(name)
  if x != null then x else default

def linesFromFile(fileName: Text) = io.Source.fromFile(fileName).getLines()
def textFromFile(fileName: Text) = linesFromFile(fileName).mkString("\n")

def dataLinesFromFile(fileName: Text) = io.Source.fromFile(fileName)
  .getLines().map(_.trim).filter(_.nonEmpty).toVector
  .filterNot(_.startsWith("#")).filterNot(_.startsWith("%"))

def recordsFromFile(fileName: Text) = dataLinesFromFile(fileName).
  map(recordFromLine(_))

def runCommand(command: Text, args: Text=""): Process =
  val proc = new ProcessBuilder(command, args)
  val INHERIT = ProcessBuilder.Redirect.INHERIT
  proc.redirectOutput(INHERIT)
  proc.redirectError(INHERIT)
  proc.start

def showTimingResults(count: Int, elapsedTime: Long) =
  printlnx("\n  total number of tests:", count)
  printlnx("  time per test: %1.1f ms\n".format(elapsedTime.toDouble/count))

def printStackTrace = new Exception().printStackTrace

lazy val ncores = Runtime.getRuntime().availableProcessors()
def printNumCores = println(s"\n$ncores processor cores available")

def currentTimeToText: Text =
  java.text.SimpleDateFormat("HH:mm:ss").format(java.util.Date())

val nullFile = if (util.Properties.isWin) "nul" else "/dev/null"
