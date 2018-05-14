
/****************************************************************************
Class CommandLine by Russ Paielli <Russ.Paielli@gmail.com> parses
command-line arguments in a style similar to scala method calls with
named arguments. For example, a program could be invoked with the
command line

  myProgram x=123 22 zz=blah 43.5 y=t gg rr=3.445

The space-delimited fields can be either assignment-style options (if
"=" is used with no spaces) or regular arguments. The regular and
assignment-style options can be interwoven arbitrarily. The
assignment-style options are accessed by the name on the left side of
the assignment (a String), and regular arguments are accessed by an
integer index (starting with 1). Methods are provided to read arguments
of type Int, Real (Double), Text (String), and Bool (Boolean). Bools can
be specified as "t" for "true" or "f" for "false".

The user can specify an optional list of valid options if desired, in
which case an Exception will be thrown if an invalid option is
detected. This catches misspellings on the command line. Alternatively,
the method "warnOfUnusedOptions" will accomplish essentially the same
thing as using an explicit list of valid options. However, it must be
called after all the command-line arguments are read in the client
program.

Here is an example of its usage (assuming the command line shown above):

  def main(args: Array[Text]) {

    val options = Set("x", "y", "zz", "rr", "k", "a") // valid options

    val com = CommandLine(args, options) // "options" is optional!

    val x = com.Int("x") // read "x" and convert it to an integer
    val k = com.Int("k", -33) // -33 is the default value if "k" is not found
    val rr = com.Real("rr") // read "rr" and convert it to a Real (Double)
    val zz = com.Text("zz") // read "zz" as Text
    val a2 = com.Real(2) // regular argument accessed with Int index
    val y = com.Bool("y")
    val a1 = com.Int(1) // regular argument with no default
    val a3 = com.Int(5, 22) // regular argument with default value 22

    com.warnOfUnusedOptions // list of valid options not needed!
    }

************************************************************************** */

package tools

import types._

case class CommandLine(array: Array[Text]=Array(), options: Set[Text]=Set(),
    numArgsReq: Int=0)
  {
  val line = array.mkString(" ").trim // reconstructed command line
  val args = array.toVector.filterNot(_.contains("="))
  val opts = array.filter(_.contains("=")).map(_.split("="))
    .map(x => (x(0), x(1))).toMap

  checkOptions

  val argCount = args.length // number of non-assignment arguments

  if (argCount < numArgsReq) { printlnx(s"\n$numArgsReq arg(s) needed"); stop }

  protected var optsUsed = Map[Text, Text]() // options used in the program
  protected var argsUsed = Set[Text]() // arguments used in the program
  protected var nonDefArgs = Map[Text, Text]() // non-default arguments

  protected def checkOptions() {
    // check whether options given on command line are in the list of
    // valid options (if provided)

    if (options.isEmpty) return // empty list of valid options, skip check

    def message(key: Text) = s"Invalid command-line option: $key" +
      s"\n\nvalid options: $options\n"

    for ((key, _) <- opts if not(options(key)))
      throw new RuntimeException(message(key))
    }

  protected def isSet(opt: Text): Bool = opts contains opt

  protected def boolx(str: Text): Text = str match { // shorthand for boolean

    case "t" => "true"
    case "f" => "false"

    case _ => str
    }

  override def toString: Text = {

    val txt = TextBuilder("\nConfig:")

    for ((name, value) <- optsUsed)
      txt ++= "\n  " + name + " = " + value

    for (value <- argsUsed)
      txt ++= "\n  <arg> = " + value

    txt
    }

  def nonDefaultArgs: Text = {

    val txt = TextBuilder("\nnonDefaultArgs:")

    for ((name, value) <- nonDefArgs)
      txt ++= "\n  " + name + " = " + value

    txt
    }

  def warnOfUnusedOptions() { warnOfUnusedOptionsExcept() }

  def warnOfUnusedOptionsExcept(ignore: Text*) {
    warnOfUnusedOptionsExcept(ignore.toSet) }

  def warnOfUnusedOptionsExcept(ignore: Set[Text]) {
    // warn of all options on command line that are not used in the
    // program (possibly due to typos on command line) except for
    // the ones in the "ignore" argument. Call after all options
    // are read into the program.

    for (opt <- opts.keys) {
      if (not(optsUsed.contains(opt)) && not(ignore(opt))) {
        val msg = "\nWARNING: Invalid or unused command-line option: " +
          opt + "\n\nvalid options: " + optsUsed.keys + "\n"
        System.err.println(msg)
        //throw new RuntimeException(msg)
        }
      }

    if (argCount > argsUsed.size) {
      val msg = TextBuilder("\nWARNING: ")
      msg ++= "unused command-line argument(s): "
      //for (x <- args.drop(argsUsed.length)) msg ++= x + " "
      msg ++= args.drop(argsUsed.size).foldLeft("")(_+_+", ")
      System.err.println(msg)
      }
    }

  // read and record assignment-style options:

  protected def record[T](option: Text, default: T, value: T): T = {
    optsUsed += option -> types.Text(value)
    if (isSet(option)) nonDefArgs += option -> types.Text(value) // <-- wrong
    value
    }

  def Text(option: Text, default: Text=""): Text = // read Text
    record[Text](option, default, if (isSet(option)) opts(option)
      else default)

  def Int(option: Text, default: Int=0): Int = // read an Int
    record[Int](option, default, if (isSet(option)) toInt(opts(option))
      else default)

  def Real(option: Text, default: Real=0): Real = // read a Real (Double)
    record[Real](option, default, if (isSet(option)) toReal(opts(option))
      else default)

  def Bool(option: Text, default: Bool=false): Bool = // read a Bool
    record[Bool](option, default, if (isSet(option))
      toBool(boolx(opts(option))) else default)

  // read/record regular args, with default if no value given on com line:

  protected def record[T](value: T): T = {
    argsUsed += types.Text(value)
    value
    }

  def Text(i: Int, default: Text) =
    record[Text](if (argCount >= i) args(i-1) else default)

  def Int(i: Int, default: Int) =
    record[Int](if (argCount >= i) toInt(args(i-1)) else default)

  def Real(i: Int, default: Real) =
    record[Real](if (argCount >= i) toReal(args(i-1)) else default)

  def Bool(i: Int, default: Bool) =
    record[Bool](if (argCount >= i) toBool(boolx(args(i-1)))
      else default)

  // read/record regular args, with no default (mandatory arguments):

  def Text(i: Int) = record[Text](args(i-1))
  def Int(i: Int) = record[Int](toInt(args(i-1)))
  def Real(i: Int) = record[Real](toReal(args(i-1)))
  def Bool(i: Int) = record[Bool](toBool(boolx(args(i-1))))
  }
