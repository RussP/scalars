
/****************************************************************************
Plotting utilities by Russ Paielli based on the free GRACE plotting
program from "http://plasma-gate.weizmann.ac.il/Grace"
************************************************************************** */

package tools_

import types_._
import mathx_._
import scalar_._

import java.io.PrintWriter
import scala.reflect.Selectable.reflectiveSelectable

class Plot(fileName: Text=nullFile, title: Text="", subtitle: Text="",
  xlabel: Text="x", ylabel: Text="y", xunit: Scalar=1, yunit: Scalar=1,
  xref: Scalar=0, yref: Scalar=0, xMargin: Scalar=0, rightMargin: Scalar=0,
  yMargin: Scalar=0, topMargin: Scalar=0, grid: Bool=false,
  equalAxes: Bool=false, copyAxes: Option[Plot]=None
  )
  extends PrintWriter(fileName):

  protected case class Objectx(pos: Vector[Scalar], txt: Text)
  protected var targets = Set[Int](10) // target (curve) numbers in use
  protected var objects = List[Objectx]() // geometric and text objects
  protected var ticksSet = false

  protected var xmin =  1e20
  protected var xmax = -1e20
  protected var ymin =  1e20
  protected var ymax = -1e20

  if grid then addGrid

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
    ylabel: Text="") =

    if title.nonEmpty then this.title(title)
    if subtitle.nonEmpty then this.subtitle(subtitle)

    if xlabel.nonEmpty then println("@xaxis label \"" + xlabel + "\"")
    if ylabel.nonEmpty then println("@yaxis label \"" + ylabel + "\"")

  setLabels(title, subtitle, xlabel, ylabel)

  def target(s: Plot.TargetStyle): Unit =
    target(s.lineStyle, s.lineWidth, s.symbol, s.size, s.color, s.legend)

  def target(lineStyle: Text="solid", lineWidth: Real=1, symbol: Text="",
    size: Real=0.5, color: Text="black", legend: Text="",
    putInBack: Bool=false): Unit =

    val target1 = if (putInBack) targets.min - 2 else targets.max + 2

    targets += target1 // save record of target number

    val t1 = "@s" + target1

    printlnx("\n" + t1, "line linestyle", lineCode(lineStyle))
    printlnx(t1, "linewidth", lineWidth)
    printlnx(t1, "symbol", symbolCode(symbol))
    printlnx(t1, "line color", colorCode(color))
    printlnx(t1, "symbol color", colorCode(color))
    printlnx(t1, "symbol size", size)
    if legend != "" then printlnx(t1, "legend", "\"" + legend + "\"")
    printlnx("@target", "g0.s" + target1 + "\n")

  protected def xmap(x: Scalar) = Real((x - xref) / xunit)
  protected def ymap(y: Scalar) = Real((y - yref) / yunit)

  protected def xmapinv(x: Real) = x * xunit + xref
  protected def ymapinv(y: Real) = y * yunit + yref

  def inFrame(v: Vector[Scalar]): Bool = inFrame(v.x, v.y)
  def inFrame(x: Scalar, y: Scalar): Bool =
    if xmap(x) < xmin then false else
    if xmap(x) > xmax then false else
    if ymap(y) < ymin then false else
    if ymap(y) > ymax then false else
    true

  def scaleToPoint(x: Scalar, y: Scalar) =

    val xval = xmap(x)
    val yval = ymap(y)

    xmin = math.min(xval, xmin)
    xmax = math.max(xval, xmax)
    ymin = math.min(yval, ymin)
    ymax = math.max(yval, ymax)

  protected def addPoint(x: Scalar, y: Scalar)
    (implicit fmt: Text, rescale: Bool=true) =

    val xval = xmap(x)
    val yval = ymap(y)

    if rescale then
      xmin = math.min(xval, xmin)
      xmax = math.max(xval, xmax)
      ymin = math.min(yval, ymin)
      ymax = math.max(yval, ymax)

    if fmt.nonEmpty then printlnx(fmt.form(xval, yval)) else
    printlnx(xval, yval)

  def Point(x: Scalar, y: Scalar)(implicit fmt: Text="", rescale: Bool=true) =
    addPoint(x, y)(fmt, rescale)

  def point(p: Vector[Scalar])(implicit fmt: Text="", rescale: Bool=true) =
    addPoint(p.x, p.y)(fmt, rescale)

  def plot1point(p: Vector[Scalar], symbol: Text="+", size: Real=1,
    color: Text="black", rescale: Bool=true) =
    target(symbol=symbol, size=size, color=color)
    point(p)(rescale=rescale)

  def plot1Point(x: Scalar=0, y: Scalar=0, symbol: Text="+", size: Real=1,
    color: Text="black", rescale: Bool=true) =
    target(symbol=symbol, size=size, color=color)
    Point(x, y)(rescale=rescale)

  def lineCode(style: Text): Int =

    val lineCode = Map("" -> 0, "none" -> 0, "solid" -> 1, "dot" -> 2,
      "dash" -> 3, "longdash" -> 4, "dotdash" -> 5)

    if not(lineCode.contains(style)) then
      System.err.println("\nWARNING: invalid line style: " + style)

    lineCode.getOrElse(style, 1)

  def symbolCode(symbol: Text): Int =

    val symbolCode = Map("" -> 0, "none" -> 0, "circle" -> 1, "o" -> 1,
      "square" -> 2, "diamond" -> 3, "uptriangle" -> 4,
      "downtriangle" -> 6, "+" -> 8, "plus" -> 8, "x" -> 9,
      "*" -> 10)

    if not(symbolCode.contains(symbol)) then
      System.err.println(s"\nWARNING: invalid symbol: $symbol")

    symbolCode.getOrElse(symbol, 1)

  def colorCode(color: Text): Int =

    val colorCode = Map("black" -> 1, "red" -> 2, "blue" -> 4,
      "gray" -> 7, "green" -> 15, "darkgray" -> 80, "darkred" -> 90)

    if not(colorCode.contains(color)) then
      System.err.println(s"\nWARNING: invalid color: $color")

    colorCode.getOrElse(color, 1)

  def setxmin(x: Scalar) = { xmin = xmap(x) }
  def setxmax(x: Scalar) = { xmax = xmap(x) }
  def setymin(y: Scalar) = { ymin = ymap(y) }
  def setymax(y: Scalar) = { ymax = ymap(y) }

  def setxRange(x0: Scalar, x1: Scalar) =
    setxmin(x0)
    setxmax(x1)

  def setyRange(y0: Scalar, y1: Scalar) =
    setymin(y0)
    setymax(y1)

  def setAxisRanges(x0: Scalar, y0: Scalar, x1: Scalar, y1: Scalar) =
    setxmin(x0)
    setxmax(x1)
    setymin(y0)
    setymax(y1)

  private def copyAxesFrom(that: Plot): Plot =
    setxmin(that.xmapinv(that.xmin))
    setxmax(that.xmapinv(that.xmax))
    setymin(that.ymapinv(that.ymin))
    setymax(that.ymapinv(that.ymax))
    this

  def axisRanges = (xmapinv(xmin), ymapinv(ymin), xmapinv(xmax), ymapinv(ymax))

  def addGrid =

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

  def setxTicks(major: Real, minor: Int=0) =
    println("\n@xaxis tick major " + major)
    println("@xaxis tick minor ticks " + minor)
    ticksSet = true

  def setyTicks(major: Real, minor: Int=0) =
    println("\n@yaxis tick major " + major)
    println("@yaxis tick minor ticks " + minor)
    ticksSet = true

  def setTicks = // this may need to be extended for wider ranges

    val (xspace, xminor) = Plot.setTicks(xmax - xmin)
    val (yspace, yminor) = Plot.setTicks(ymax - ymin)

    setxTicks(xspace, xminor)
    setyTicks(yspace, yminor)

    ticksSet = true

  private def setAxisScalesEqual: Unit =

    var xmin = this.xmin
    var xmax = this.xmax
    var ymin = this.ymin
    var ymax = this.ymax

    if xmin >= xmax then return
    if ymin >= ymax then return

    var xdif = xmax - xmin
    var ydif = ymax - ymin

    val xmid = (xmin + xmax) / 2
    val ymid = (ymin + ymax) / 2

    if xdif > 1.6 * ydif then
      ydif = xdif / 1.6
      ymin = ymid - ydif / 2
      ymax = ymin + ydif
    else
      xdif = ydif * 1.6
      xmin = xmid - xdif / 2
      xmax = xmin + xdif

    this.xmin = xmin
    this.xmax = xmax
    this.ymin = ymin
    this.ymax = ymax

  def line1( // draw a line
    p0: { val x: Scalar; val y: Scalar },
    p1: { val x: Scalar; val y: Scalar },
    rescale: Bool=false, style: Text="solid", color: Text="black",
    width: Real=1, arrow: Int=0, arrowtype: Int=1, arrowlength: Real=2) =

    line(p0.x, p0.y, p1.x, p1.y, rescale=rescale, style=style, color=color,
      width=width, arrow=arrow, arrowtype=arrowtype,
      arrowlength=arrowlength)

  def line(x0: Scalar, y0: Scalar, x1: Scalar, y1: Scalar,
    rescale: Bool=false, style: Text="solid", color: Text="black",
    width: Real=1, arrow: Int=0, arrowtype: Int=1, arrowlength: Real=2) =
    // draw a line (with optional arrow)

    implicit val rescale1 = rescale
    target(lineStyle=style, lineWidth=width, color=color)
    Point(x0, y0)
    Point(x1, y1)

    if arrow > 0 then
      val dir = atan2(x1-x0, y1-y0)
      this.arrow(x1, y1, dir=dir, color=color, width=width)

  def arrow(x0: Scalar, y0: Scalar, dir: Scalar, color: Text="black", 
    width: Real=1, arrowtype: Int=1, arrowlength: Real=2) =
    // add an arrow at specified position (uses very short line)
    val eps = 1e-4 // short line length
    val x1 = x0 - eps * xunit * sin(dir) // direction (dir) clockwise from north
    val y1 = y0 - eps * yunit * cos(dir)
    drawArrow(x0, y0, x1, y1, color=color, width=width, arrowtype=arrowtype,
        arrowlength=arrowlength)

  def arrow1(pos: { val x: Scalar; val y: Scalar }, dir: Scalar,
    color: Text="black", width: Real=1, arrowtype: Int=1, arrowlength: Real=2) =
    arrow(pos.x, pos.y, dir, color=color, width=width, arrowtype=arrowtype,
        arrowlength=arrowlength)

  private def drawArrow(x0: Scalar, y0: Scalar, x1: Scalar, y1: Scalar,
    rescale: Bool=false, style: Text="solid", color: Text="black",
    width: Real=1, arrowtype: Int=1, arrowlength: Real=2): Unit =

    val arrow1 =
      s"@line arrow type $arrowtype\n" +
      s"@line arrow length $arrowlength\n" +
      s"@line arrow layout 0.6, 0.5\n"

    val txt = "\n@with line\n" +
      s"@line on\n" +
      s"@line loctype world\n" +
      s"@line linestyle ${lineCode(style)}\n" +
      s"@line arrow 2\n" + arrow1 + // 2 for forward arrow (1 for backward)
      s"@line color ${colorCode(color)}\n" +
      s"@line linewidth $width\n" +
      s"@line g0\n" +
      s"@line " +
      fmt.form(xmap(x0)) + "," +
      fmt.form(ymap(y0)) + "," +
      fmt.form(xmap(x1)) + "," +
      fmt.form(ymap(y1)) + "\n" +
      "@line def\n"

    objects :+= Objectx(Vector(x0, y0), txt)

  def box(x1: Scalar, y1: Scalar, x2: Scalar, y2: Scalar, rescale: Bool=false,
    lineStyle: Text="solid", lineWidth: Real=1.0, color: Text="black") =
    // draw a rectangle

    implicit val rescale1 = rescale
    target(lineStyle=lineStyle, lineWidth=lineWidth, color=color)
    Point(x1, y1)
    Point(x1, y2)
    Point(x2, y2)
    Point(x2, y1)
    Point(x1, y1)

  def circle(xc: Scalar, yc: Scalar, radius: Scalar, rescale: Bool=false,
    style: Text="solid", color: Text="black", width: Real=1,
    angleInc: Scalar=10*Pi/180) = // draw a circle

    target(lineStyle=style, lineWidth=width, color=color)
    for ang <- scalarStepsx(0, TwoPi, angleInc) do
      Point(xc + radius * sin(ang), yc + radius * cos(ang))(rescale=rescale)

  def circle1(center: { val x: Scalar; val y: Scalar }, radius: Scalar,
    style: Text="solid", color: Text="black") = // draw a circle
    circle(center.x, center.y, radius, style=style, color=color)

  def text1(txt: Text, p: { val x: Scalar; val y: Scalar }, color: Text="black",
    loctype: Text="world", size: Real=1, just: Int=0) = // print text on plot
    text(txt, p.x, p.y, color=color, loctype=loctype, size=size, just=just)

  def text(text: Text, x: Scalar=0, y: Scalar=0, color: Text="black",
    loctype: Text="world", size: Real=1, just: Int=0,
    outOfFrame: Bool=false) = // print text on plot

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

    if outOfFrame then println(txt) else
    objects :+= Objectx(Vector(x, y), txt)

  def legend(x: Real=0.2, y: Real=0.8, charsize: Real=0.8) =

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

  override def flush = super.flush

  override def close =

    if xmin > xmax || ymin > ymax then plot1Point()
      //System.out.println(s"\nWARNING: negative axis range in $fileName\n")

    if copyAxes.nonEmpty then copyAxesFrom(copyAxes.get) else
      xmin -= Real(xMargin / xunit)
      xmax += Real(max(xMargin, rightMargin) / xunit)
      ymin -= Real(yMargin / yunit)
      ymax += Real(max(yMargin, topMargin) / yunit)
      if equalAxes then setAxisScalesEqual

    printlnx("\n@autoscale onread none")
    printlnx("@world xmin ", xmin)
    printlnx("@world xmax ", xmax)
    printlnx("@world ymin ", ymin)
    printlnx("@world ymax ", ymax)
    printlnx("@autoticks")

    for o <- objects if inFrame(o.pos) do println(o.txt)
    if not(ticksSet) then setTicks

    super.close

  def close1: Plot = { close; this } // close and return this plot
  def closeIf(b: Bool): Plot = { if b then close; this }

object Plot:

  case class TargetStyle(lineStyle: Text="solid", lineWidth: Real=1,
    symbol: Text="", size: Real=0.5, color: Text="black", legend: Text="")

  def setTicks(length: Real): (Real, Int) =

    import math._

    val length1 = if (length > 0) length else 1.0
    val exp = Int(log10(length1)+1000)-1000
    val scale = pow(10, exp)
    val mant = length1 / scale // mantissa

    if mant > 8 then (2 * scale, 1) else
    if mant > 4 then (1 * scale, 0) else
    if mant > 1.8 then (0.5 * scale, 0) else
    //if mant > 1.5 then (0.5 * scale, 4) else
    (0.2 * scale, 0)
    //printlnx("\nPlot.scala", length, exp, scale, mant, xx); stop
