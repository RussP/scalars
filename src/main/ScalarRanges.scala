
package scalar_

import types_._
import mathx_._

case class ScalarRange(low: Scalar=0, high: Scalar=0):

  val valid = high >= low
  if not(valid) then printlnx(s"\nScalarRange err: low($low) > high($high)")
  require(valid)

  def dif = high - low
  def hasOverlapWith(that: ScalarRange) = separation(that) == 0

  def contains(s: Scalar) = s >= low && s <= high
  def containsNot(s: Scalar) = not(contains(s))

  def contains(that: ScalarRange): Bool = low <= that.low && high >= that.high
  def containsNot(that: ScalarRange): Bool = not(contains(that))

  def isContainedBy(that: ScalarRange): Bool = that.contains(this)
  def notContainedBy(that: ScalarRange): Bool = not(isContainedBy(that))

  def isContainedBy(that: ScalarRanges): Bool = that.contains(this)
  def notContainedBy(that: ScalarRanges): Bool = not(isContainedBy(that))

  def separation(that: ScalarRange): Scalar =
    if that.low > high then that.low - high else
    if low > that.high then low - that.high else 0

  def intersection(that: ScalarRange): Option[ScalarRange] =
    if that.low > high then return None
    if that.high < low then return None
    val lo = max(that.low, low)
    val hi = min(that.high, high)
    Option(ScalarRange(lo, hi))

  def + (s: Scalar) = ScalarRange(low+s, high+s)
  def - (s: Scalar) = ScalarRange(low-s, high-s)
  def * (s: Scalar) = ScalarRange(low*s, high*s)
  def / (s: Scalar) = ScalarRange(low/s, high/s)

  def txt(fmt: Text="%1.6g"): Text =
    s"ScalarRange(${fmt.form(low)}, ${fmt.form(high)})"

  override def toString: Text = txt()

case class ScalarRanges(input: Vector[ScalarRange] = Vector()):

  lazy val ranges = ScalarRanges.combineRanges(input)

  override def toString = s"ScalarRanges($ranges)"

  def isEmpty = input.isEmpty
  def nonEmpty = input.nonEmpty
  def length = ranges.length

  def lowOption  = if (nonEmpty) Option(ranges.head.low) else None
  def highOption = if (nonEmpty) Option(ranges.last.high) else None

  def lowOrElse (x: Scalar) = if (nonEmpty) ranges.head.low else x
  def highOrElse(x: Scalar) = if (nonEmpty) ranges.last.high else x

  def foreach(f: ScalarRange=>Unit) = ranges.foreach(f)

  def take(n: Int) = copy(ranges.take(n))
  def drop(n: Int) = copy(ranges.drop(n))
  def takeRight(n: Int) = copy(ranges.takeRight(n))
  def dropRight(n: Int) = copy(ranges.dropRight(n))

  def +: (range: ScalarRange) = copy(range +: ranges)
  def :+ (range: ScalarRange) = copy(ranges :+ range)
  def ++ (ranges1: ScalarRanges) = copy(ranges ++ ranges1.ranges)
  def ++ (ranges1: Vector[ScalarRange]) = copy(ranges ++ ranges1)
  def ++ (ranges1: Option[ScalarRange]) = copy(ranges ++ ranges1)

  def + (s: Scalar) = copy(ranges.map(_ + s))
  def - (s: Scalar) = copy(ranges.map(_ - s))
  def * (s: Scalar) = copy(ranges.map(_ * s))
  def / (s: Scalar) = copy(ranges.map(_ / s))

  def contains(s: Scalar): Bool =
    for range <- ranges if range.contains(s) do return true
    false

  def contains(s: ScalarRange): Bool =
    for range <- ranges if range.contains(s) do return true
    false

  def containsNot(s: Scalar) = not(contains(s))
  def containsNot(s: ScalarRange) = not(contains(s))

  def contains(that: ScalarRanges): Bool =
    for range2 <- that.ranges do
      var contained = false
      for (range1 <- ranges) if (range1.contains(range2)) contained = true
      if not(contained) then return false
    true

  def isContainedBy(that: ScalarRanges): Bool = that.contains(this)
  def notContainedBy(that: ScalarRanges): Bool = not(isContainedBy(that))

  def intersection(that: ScalarRanges): Option[ScalarRanges] =
    val temp = for (r1 <- ranges; r2 <- that.ranges) yield r1 intersection r2
    if temp.isEmpty then None else
    Option(ScalarRanges(temp.flatten))

  def complement(floor: Scalar, ceiling: Scalar): ScalarRanges =
    // open ranges from floor to ceiling

    if isEmpty then return ScalarRanges(Vector(ScalarRange(floor, ceiling)))

    val low = lowOption.get
    val high = highOption.get

    var output = Vector[ScalarRange]()

    if floor < low then output :+= ScalarRange(floor, low)
    for (Vector(r1, r2) <- ranges.sliding(2))
      output :+= ScalarRange(r1.high, r2.low)
    if ceiling > high then output :+= ScalarRange(high, ceiling)

    ScalarRanges(output)

object ScalarRanges:

  def noOverlap(ranges: Vector[ScalarRange]) = not(rangesOverlap(ranges))

  def rangesOverlap(ranges: Vector[ScalarRange]): Bool =
    for r1 <- ranges; r2 <- ranges if r2.low < r1.low do
      if r1 hasOverlapWith r2 then return true
    false

  def combineRanges(ranges: Vector[ScalarRanges]): ScalarRanges =
    ScalarRanges(ranges.map(_.ranges).flatten)

  def combineRanges(ranges: Vector[ScalarRange]): Vector[ScalarRange] =
    // merge ranges into non-overlapping ranges based on
    // https://www.geeksforgeeks.org/merging-intervals

    if ranges.isEmpty then return ranges

    val ranges1 = ranges.sortBy(_.low)
    var output = ranges1.take(1) // output to be constructed

    for range <- ranges1.drop(1) do
      val last = output.last
      if range.low > last.high then output :+= range
      else if (last.high < range.high)
        output = output.dropRight(1) :+ last.copy(high=range.high)

    //assert(noOverlap(output))
    output
