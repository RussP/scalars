
/****************************************************************************
Class ConfigReader by Russ Paielli <Russ.Paielli@gmail.com> reads a an
optional configuration text file if available and sets parameters
accordingly. If the configuration file is not found (or is empty), the
program continues uninterrupted. It provides a method to check that all
provided paramters are read, which is useful for detecting typos in the
configuration file. The configuration text file should contain plain
assignment statements (with no val/var), and it can use any units that
you might define if you use my Scalar class (available at
http://RussP.us/scalar-scala.htm). Here's a simple example:

numberOfWidgets = 3
widgetWeight = 20 * kg

************************************************************************** */

package tools_

import types_._
import scalar_._

case class ConfigReader(fileName: Text):

  protected var map = Map[Text, Text]() // map of config parameters to values
  protected var parsRead = Set[Text]() // parameters read in the program

  val configFile = new java.io.File(fileName)

  if configFile.exists then

    val source = scala.io.Source.fromFile(configFile)

    for line <- source.getLines().map(_.trim).
      filterNot(_.startsWith("#")).map(_.split("//")(0));
      pair <- line.split(";").map(_.trim).filter(_.contains("=")).
      map(_.split("=").map(_.trim)) do

      val (key, value) = (pair(0), pair(1))
      map += key -> value

    source.close

  def empty = map.empty
  def nonEmpty = map.nonEmpty

  override def toString: Text =
    val txt = TextBuilder(s"\n$fileName:\n")
    if configFile.exists then for (key <- map.keys.toList.sorted)
      txt ++= ("  " + key + " = " + map(key) + "\n")
    txt

  def copyInputFile(testName: Text) =

    import java.nio.file._
    import java.nio.file.StandardCopyOption._

    if configFile.exists then
      val from = Paths.get(fileName)
      val to = Paths.get(testName + "-" + fileName)
      Files.copy(from, to, REPLACE_EXISTING)

  protected def isSet(param: Text) =
    parsRead += param
    map.contains(param)

  protected def valueOf(key: Text): Text = // return the value of the key
    if not(isSet(key)) then
      val msg = "ConfigReader parameter not found: " + key
      throw new RuntimeException(msg)
    map(key)

  def Text(key: Text): Text = valueOf(key) // read a String, no default
  def Int(key: Text): Int = toInt(valueOf(key)) // read an Int, no default
  def Real(key: Text): Real = toReal(valueOf(key)) // read a Double, no default
  def Bool(key: Text): Bool = toBool(valueOf(key)) // read a Bool, no default

  def Scalar(key: Text): Scalar = // read a Scalar, no default

    val pair = valueOf(key).replace("*"," ").split(" +")

    val num = toReal(pair(0))
    val unit = nameToUnit(pair(1))

    num * unit

  def Text(key: Text, default: Text): Text = // read a String, with default
    if isSet(key) then Text(key) else default

  def Int(key: Text, default: Int): Int = // read an Int, with default
    if isSet(key) then Int(key) else default

  def Real(key: Text, default: Real): Real = // read a Double, with default
    if isSet(key) then Real(key) else default

  def Bool(key: Text, default: Bool): Bool = // read a Bool, with default
    if isSet(key) then Bool(key) else default

  def Scalar(key: Text, default: Scalar): Scalar = // read a Scalar, with default
    if isSet(key) then Scalar(key) else default

  def checkConfigPars(abort: Bool, ignore: Text*) =
    // warn of all configuration parameters that are not read into
    // the program (possibly due to typos in the configuration
    // file). Call this after all parameters are read into program.

    val ignored = ignore.toSet

    for opt <- map.keys if not(parsRead(opt)) && not(ignored(opt)) do
      val txt = "\n\nERROR: Invalid or unused config parameter: "
      val msg = txt + opt + "\n\nvalid parameters: " + parsRead + "\n"
      if abort then throw new RuntimeException(msg)
      else System.err.println(msg)
