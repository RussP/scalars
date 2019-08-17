
import language.implicitConversions

package object types_ { // type aliases, conversions, and other useful stuff

  type Real = Double // better name for Double
  type Bool = Boolean // shorter name for Boolean
  type Text = String // better name for String

  def not(b: Bool) = ! b // clearer form of not!
  def and(a: Bool, b: => Bool) = a && b // I don't use this, but it's there
  def or (a: Bool, b: => Bool) = a || b // ditto
  def xor(a: Bool, b: Bool) = (a && !b) || (b && !a)

  def Text(x: Any) = x.toString

  def toInt (t: Text) = t.toInt // better form for conversion
  def toLong(t: Text) = t.toLong // ditto
  def toReal(t: Text) = t.toDouble // ditto
  def toBool(t: Text) = t.toBoolean // ditto

  def truncToInt (r: Real) = r.toInt // truncate real to integer
  def truncToLong (r: Real) = r.toLong // truncate real to long integer

  def TextBuilder(t: Text="") = new StringBuilder(t) // better name!
  implicit def StringBuilderToText(t: StringBuilder) = Text(t)

  type VList[T] = Vector[T] // another name for Vector
  def VList[T](x: T*) = Vector[T](x: _*)

  def enter( // prompt user to enter input
      label: Text = "", // label for requested input
      default: Text = ""): // default value to return if nothing entered
    Text = {

    print("enter " + label + " [" + default + "]: ")
    val entry = scala.io.StdIn.readLine
    if (entry == "") default else entry
    }

  def stop() = { // breakpoint for debugging
    print("\nhit <enter> to continue ")
    scala.io.StdIn.readLine
    ()
    }

  def getenvx(name: Text, default: Text) = {
    // get environment variable or default if not set
    val x = System.getenv(name)
    if (x != null) x else default
    }

  def linesFromFile(fileName: Text) = io.Source.fromFile(fileName).getLines
  def textFromFile(fileName: Text) = linesFromFile(fileName).mkString("\n")

  def dataLinesFromFile(fileName: Text) = io.Source.fromFile(fileName)
    .getLines.map(_.trim).filter(_.nonEmpty).toVector
    .filterNot(_.startsWith("#")).filterNot(_.startsWith("%"))

  def splitLine(line: Text) = line.trim.split(" *[ ,] *")
  def recordFromLine(line: Text) = splitLine(line).toVector

  def recordsFromFile(fileName: Text) = dataLinesFromFile(fileName).
    map(recordFromLine(_))

  def printx(a: Any*)(implicit s: Text=" ") = print(a.mkString(s))
  def printlnx(a: Any*)(implicit s: Text=" ") = println(a.mkString(s))

  def runCommand(command: Text, args: Text=""): Process = {
    val proc = new ProcessBuilder(command, args)
    val INHERIT = ProcessBuilder.Redirect.INHERIT
    proc.redirectOutput(INHERIT)
    proc.redirectError(INHERIT)
    proc.start
    }

  lazy val ncores = Runtime.getRuntime().availableProcessors()
  def printNumCores() = println(s"\n$ncores processor cores available")

  val nullFile = if (util.Properties.isWin) "nul" else "/dev/null"
  }
