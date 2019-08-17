
/****************************************************************************
Plotting utilities by Russ Paielli based on the free GRACE plotting
program obtained from "http://plasma-gate.weizmann.ac.il/Grace"
************************************************************************** */

package tools_

import types_._
import scalar_._
import Vectorx._

import java.io.PrintWriter
import scala.language.reflectiveCalls

object Plot {

  case class TargetStyle(lineStyle: Text="solid", lineWidth: Real=1,
      symbol: Text="", size: Real=0.5, color: Text="black", legend: Text="")

  def setTicks(length: Real): (Real, Int) = {

    import math._

    val length1 = if (length > 0) length else 1.0
    val exp = Int(log10(length1)+1000)-1000
    val scale = pow(10, exp)
    val mant = length1 / scale

    if (mant > 8) (2 * scale, 1) else
    if (mant > 4) (1 * scale, 1) else
    if (mant > 2.2) (0.5 * scale, 0) else
    if (mant > 1.5) (0.5 * scale, 4) else
    (0.2 * scale, 1)
    }
  }

protected case class Objectx(pos: Vector[Scalar], txt: Text) // for geometric objects/text

class Plot(fileName: Text, title: Text="", subtitle: Text="",
    xlabel: Text="x", ylabel: Text="y", xunit: Scalar=1, yunit: Scalar=1,
    xref: Scalar=0, grid: Bool=false)
  extends PrintWriter(fileName) {

  protected var xmin =  1e20
  protected var xmax = -1e20
  protected var ymin =  1e20
  protected var ymax = -1e20

  protected var xRangeSet = false
  protected var yRangeSet = false

  protected var targets = Set[Int](10) // target (curve) numbers in use

  protected var objects = List[Objectx]() // geometric and text objects
  protected var ticksSet = false

  if (grid) addGrid

  val title1 = title
  val subtitle1 = subtitle

  val fmt = "%1.8g" // format for placing geometric objects

  println("@map color 80 to (150, 150, 150), \"darkgray\"")
  println("@map color 90 to (220, 0, 0), \"darkred\"")

  def xrange = xmax - xmin
  def yrange = ymax - ymin

  def printx(a: Any*) = print(a.mkString(" "))
  def printlnx(a: Any*) = println(a.mkString(" "))

  def title(txt: Text) = printlnx("\n@title \"" + txt + "\"")
  def subtitle(txt: Text) = printlnx("\n@subtitle \"" + txt + "\"")

  def subtitleAppend(txt: Text) = subtitle(s"$subtitle1 $txt")

  def setLabels(title: Text="", subtitle: Text="", xlabel: Text="",
    ylabel: Text="") = {

    if (title.nonEmpty) this.title(title)
    if (subtitle.nonEmpty) this.subtitle(subtitle)

    if (xlabel.nonEmpty) println("@xaxis label \"" + xlabel + "\"")
    if (ylabel.nonEmpty) println("@yaxis label \"" + ylabel + "\"")
    }

  setLabels(title, subtitle, xlabel, ylabel)

  def target(s: Plot.TargetStyle): Unit =
    target(s.lineStyle, s.lineWidth, s.symbol, s.size, s.color, s.legend)

  def target(lineStyle: Text="solid", lineWidth: Real=1, symbol: Text="",
    size: Real=0.5, color: Text="black", legend: Text="",
    putInBack: Bool=false): Unit = {

    val target1 = if (putInBack) targets.min - 2 else targets.max + 2

    targets += target1 // save record of target number

    val t1 = "@s" + target1

    printlnx("\n" + t1, "line linestyle", lineCode(lineStyle))
    printlnx(t1, "linewidth", lineWidth)
    printlnx(t1, "symbol", symbolCode(symbol))
    printlnx(t1, "line color", colorCode(color))
    printlnx(t1, "symbol color", colorCode(color))
    printlnx(t1, "symbol size", size)
    if (legend != "") printlnx(t1, "legend", "\"" + legend + "\"")
    printlnx("@target", "g0.s" + target1 + "\n")
    }

  def xmap(x: Scalar) = Real((x - xref) / xunit)
  def ymap(y: Scalar) = Real(y / yunit)

  def inFrame(v: Vector[Scalar]): Bool = inFrame(v.x, v.y)

  def inFrame(x: Scalar, y: Scalar): Bool = {

    if (xmap(x) < xmin) false else
    if (xmap(x) > xmax) false else
    if (ymap(y) < ymin) false else
    if (ymap(y) > ymax) false else

    true
    }

  def scaleToPoint(x: Scalar, y: Scalar) = {

    val xval = xmap(x)
    val yval = ymap(y)

    xmin = math.min(xval, xmin)
    xmax = math.max(xval, xmax)
    ymin = math.min(yval, ymin)
    ymax = math.max(yval, ymax)
    }

  protected def addPoint(x: Scalar, y: Scalar)
    (implicit fmt: Text, rescale: Bool=true) = {

    val xval = xmap(x)
    val yval = ymap(y)

    if (rescale) {
      xmin = math.min(xval, xmin)
      xmax = math.max(xval, xmax)
      ymin = math.min(yval, ymin)
      ymax = math.max(yval, ymax)
      }

    if (fmt.nonEmpty) printlnx(fmt.form(xval, yval)) else

    printlnx(xval, yval)
    }

  def Point(x: Scalar, y: Scalar)
    (implicit fmt: Text="", rescale: Bool=true) =
    addPoint(x, y)(fmt, rescale)

  def point(p: Vector[Scalar])(implicit fmt: Text="", rescale: Bool=true) =
    addPoint(p.x, p.y)(fmt, rescale)

  def plot1point(p: Vector[Scalar], symbol: Text="+", size: Real=1,
      color: Text="black") = {
    target(symbol=symbol, size=size, color=color)
    point(p)
    }

  def plot1Point(x: Scalar=0, y: Scalar=0, symbol: Text="+", size: Real=1,
      color: Text="black") = {
    target(symbol=symbol, size=size, color=color)
    Point(x, y)
    }

  def lineCode(style: Text): Int = {

    val lineCode = Map("" -> 0, "none" -> 0, "solid" -> 1, "dot" -> 2,
      "dash" -> 3, "longdash" -> 4, "dotdash" -> 5)

    if (not(lineCode.contains(style)))
      System.err.println("\nWARNING: invalid line style: " + style)

    lineCode.getOrElse(style, 1)
    }

  def symbolCode(symbol: Text): Int = {

    val symbolCode = Map("" -> 0, "none" -> 0, "circle" -> 1, "o" -> 1,
      "square" -> 2, "diamond" -> 3, "uptriangle" -> 4,
      "downtriangle" -> 6, "+" -> 8, "plus" -> 8, "x" -> 9,
      "*" -> 10)

    if (not(symbolCode.contains(symbol)))
      System.err.println(s"\nWARNING: invalid symbol: $symbol")

    symbolCode.getOrElse(symbol, 1)
    }

  def colorCode(color: Text): Int = {

    val colorCode = Map("black" -> 1, "red" -> 2, "blue" -> 4,
      "gray" -> 7, "green" -> 15, "darkgray" -> 80, "darkred" -> 90)

    if (not(colorCode.contains(color)))
      System.err.println(s"\nWARNING: invalid color: $color")

    colorCode.getOrElse(color, 1)
    }

  def setxmin(xmin: Scalar) = {
    xRangeSet = true
    this.xmin = xmap(xmin)
    printlnx("\n@world xmin ", this.xmin)
    }

  def setxmax(xmax: Scalar) = {
    xRangeSet = true
    this.xmax = xmap(xmax)
    printlnx("\n@world xmax ", this.xmax)
    }

  def setymin(ymin: Scalar) = {
    yRangeSet = true
    this.ymin = ymap(ymin)
    printlnx("\n@world ymin ", this.ymin)
    }

  def setymax(ymax: Scalar) = {
    yRangeSet = true
    this.ymax = ymap(ymax)
    printlnx("\n@world ymax ", this.ymax)
    }

  def setxRange(xmin: Scalar, xmax: Scalar) = {
    xRangeSet = true
    setxmin(xmin)
    setxmax(xmax)
    }

  def setyRange(ymin: Scalar, ymax: Scalar) = {
    yRangeSet = true
    setymin(ymin)
    setymax(ymax)
    }

  def setAxisRanges(xmin: Scalar, ymin: Scalar, xmax: Scalar, ymax: Scalar) = {

    setxmin(xmin)
    setxmax(xmax)
    setymin(ymin)
    setymax(ymax)
    }

  def setMargins(xpad: Scalar=0, ypad: Scalar=0) = {

    printlnx("\n@autoscale onread none")
    printlnx("@world xmin ", xmin - xpad/xunit)
    printlnx("@world xmax ", xmax + xpad/xunit)
    printlnx("@world ymin ", ymin - ypad/yunit)
    printlnx("@world ymax ", ymax + ypad/yunit)
    printlnx("@autoticks")
    }

  def addGrid() = {

    println
    println("@map color 70 to (200, 200, 200), \"grey\"")
    println("@map color 80 to (150, 150, 150), \"darkgrey\"")
    println
    println("@xaxis tick major grid on")
    println("@yaxis tick major grid on")
    println
    println("@xaxis tick minor grid on")
    println("@yaxis tick minor grid on")
    println
    println("@xaxis tick major color 80")
    println("@yaxis tick major color 80")
    println
    println("@xaxis tick minor color 70")
    println("@yaxis tick minor color 70")
    println
    }

  def setxTicks(major: Real, minor: Int=0) = {

    println("\n@xaxis tick major " + major)
    println("@xaxis tick minor ticks " + minor)

    ticksSet = true
    }

  def setyTicks(major: Real, minor: Int=0) = {

    println("\n@yaxis tick major " + major)
    println("@yaxis tick minor ticks " + minor)

    ticksSet = true
    }

  def setTicks() = { // this may need to be extended for wider ranges

    val (xspace, xminor) = Plot.setTicks(xmax - xmin)
    val (yspace, yminor) = Plot.setTicks(ymax - ymin)

    setxTicks(xspace, xminor)
    setyTicks(yspace, yminor)

    ticksSet = true
    }

  def setAxisScalesEqual(): Unit = {

    var xmin = this.xmin
    var xmax = this.xmax
    var ymin = this.ymin
    var ymax = this.ymax

    if (xmin >= xmax) return
    if (ymin >= ymax) return

    var xdif = xmax - xmin
    var ydif = ymax - ymin

    if (xdif == 0) xdif = 1
    if (ydif == 0) ydif = 1

    val xmid = (xmin + xmax) / 2
    val ymid = (ymin + ymax) / 2

    if (xdif > 1.6 * ydif) {
      ydif = xdif / 1.6
      ymin = ymid - ydif / 2
      ymax = ymin + ydif
      }
    else {
      xdif = ydif * 1.6
      xmin = xmid - xdif / 2
      xmax = xmin + xdif
      }

    xmin -= xdif / 20
    ymin -= xdif / 32
    xmax += xdif / 20
    ymax += xdif / 32

    this.xmin = xmin
    this.xmax = xmax
    this.ymin = ymin
    this.ymax = ymax

    val txt = "\n@autoscale onread none\n" +
      s"@world xmin $xmin\n" +
      s"@world xmax $xmax\n" +
      s"@world ymin $ymin\n" +
      s"@world ymax $ymax\n" +
      "@autoticks\n"

    println(txt)
    }

  def line1( // draw a line
    p0: { val x: Scalar; val y: Scalar },
    p1: { val x: Scalar; val y: Scalar },
    style: Text="solid", color: Text="black", width: Real=1,
    arrow: Int=0, arrowtype: Int=1, arrowlength: Real=2, label: Text="") = {

    line(p0.x, p0.y, p1.x, p1.y, style=style, color=color,
    width=width, arrow=arrow, arrowtype=arrowtype,
    arrowlength=arrowlength, label=label)
    }

  def line(x0: Scalar, y0: Scalar, x1: Scalar, y1: Scalar,
    style: Text="solid", color: Text="black", width: Real=1,
    arrow: Int=0, arrowtype: Int=1, arrowlength: Real=2, label: Text="") = {
    // draw a line

    val arrow1 = if (arrow == 0) "" else {
      s"@line arrow type $arrowtype\n" +
      s"@line arrow length $arrowlength\n" +
      s"@line arrow layout 0.6, 0.5\n"
      }

    val txt = "\n@with line\n" +
      s"@line on\n" +
      s"@line loctype world\n" +
      s"@line linestyle ${lineCode(style)}\n" +
      s"@line arrow $arrow\n" + arrow1 +
      s"@line color ${colorCode(color)}\n" +
      s"@line linewidth $width\n" +
      s"@line g0\n" +
      s"@line " +
      fmt.form(xmap(x0)) + "," +
      fmt.form(ymap(y0)) + "," +
      fmt.form(xmap(x1)) + "," +
      fmt.form(ymap(y1)) + "\n" +
      "@line def\n"

    if (label.nonEmpty) { } // to be added

    objects :+= Objectx(Vector(x0, y0), Text(txt))
    }

  def circle(xc: Scalar, yc: Scalar, radius: Scalar,
    style: Text="solid", color: Text="black") = { // draw a circle

    val txt = "\n@with ellipse\n" +
      "@ellipse on\n" +
      "@ellipse loctype world\n" +
      "@ellipse g0\n" +
      "@ellipse " +
      fmt.form(xmap(xc - radius)) + "," +
      fmt.form(ymap(yc - radius)) + "," +
      fmt.form(xmap(xc + radius)) + "," +
      fmt.form(ymap(yc + radius)) + "\n" +
      s"@ellipse linestyle ${lineCode(style)}\n" +
      s"@ellipse color ${colorCode(color)}\n" +
      s"@ellipse def\n"

    objects :+= Objectx(Vector(xc, yc), Text(txt))
    }

  def circle1(center: { val x: Scalar; val y: Scalar }, radius: Scalar,
      style: Text="solid", color: Text="black") = // draw a circle
    circle(center.x, center.y, radius)

  def text1(txt: Text, p: { val x: Scalar; val y: Scalar },
    color: Text="black", loctype: Text="world", size: Real=1,
    just: Int=0) = {

    text(txt, p.x, p.y, color=color, loctype=loctype, size=size, just=just)
    }

  def text(text: Text, x: Scalar=0, y: Scalar=0, color: Text="black",
    loctype: Text="world", size: Real=1, just: Int=0,
    outOfFrame: Bool=false) = { // print text on plot

    val (x1, y1) = if (loctype == "world") (xmap(x), ymap(y)) else (x, y)

    val txt = "\n@with string\n" +
      s"@string on\n" +
      s"@string loctype $loctype\n" +
      s"@string g0\n" +
      s"@string ${fmt.form(x1)}, ${fmt.form(y1)}\n" +
      s"@string color ${colorCode(color)}\n" +
      s"@string rot 0\n" +
      s"@string font 0\n" +
      s"@string just $just\n" +
      s"@string char size $size\n" +
      s"""@string def "$text"\n"""

    if (outOfFrame) println(txt) else

    objects :+= Objectx(Vector(x, y), Text(txt))
    }

  def legend(x: Real=0.2, y: Real=0.8, charsize: Real=0.8) = {

    val txt = "\n@legend on\n" +
      "@legend loctype view\n" +
      s"@legend $x, $y\n" +
      "@legend box color 1\n" +
      "@legend box pattern 1\n" +
      "@legend box linewidth 1.0\n" +
      "@legend box linestyle 1\n" +
      "@legend box fill color 0\n" +
      "@legend box fill pattern 1\n" +
      "@legend font 0\n" +
      s"@legend char size $charsize\n" +
      "@legend color 1\n" +
      "@legend length 4\n" +
      "@legend vgap 1\n" +
      "@legend hgap 1\n" +
      "@legend invert false\n"

    println(txt)
    }

  override def close = {

    if (xmin > xmax || ymin > ymax) plot1Point()
      //System.out.println(s"\nWARNING: negative axis range in $fileName\n")

    if (xRangeSet && yRangeSet) println("\n@autoscale onread none") else
    if (xRangeSet) println("\n@autoscale onread yaxes") else
    if (yRangeSet) println("\n@autoscale onread xaxes")

    for (o <- objects if inFrame(o.pos)) println(o.txt)
    if (not(ticksSet)) setTicks

    super.close
    }
  }
