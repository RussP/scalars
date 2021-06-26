package mathx_ // various math functions for scalars

import types_._
import scalar_._
import scala.math.Ordering.Double.IeeeOrdering // TotalOrdering also possible

val Pi = math.Pi
val TwoPi = 2 * Pi
val HalfPi = Pi / 2

def scalarSteps(start: Scalar, end: Scalar, step: Scalar, tol: Real=1e-8):
  Vector[Scalar] =

  if step == zero then { assert(end == start); return Vector(start) }

  def dec(x: Scalar) = BigDecimal(Real(x))

  val inc = abs(step)
  val sgn = dec(signum(step))
  val start1 = dec(start/inc)
  val end1 = dec((end + tol * step) / inc)

  (start1 to end1 by sgn).map(_.toDouble).map(_ * inc).toVector

  //import scala.collection.immutable.Range.BigDecimal.inclusive
  //inclusive(start1, end1, sgn).map(_.toDouble).map(_ * inc).toVector

def scalarStepsx(start: Scalar, end: Scalar, step: Scalar, tol: Real=1e-3):
  Vector[Scalar] = // scalarSteps guaranteed to include end point

  val steps = scalarSteps(start, end, step)
  if step == zero then return steps
  if steps.isEmpty then return steps

  val endDif = abs((steps.last - end) / step)
  if endDif < tol then steps else steps :+ end

def normAngle(angle: Scalar): Scalar = // equivalent angle in range [-Pi,Pi]
  val ang = angle % TwoPi
  if ang < -Pi then ang + TwoPi else
  if ang >  Pi then ang - TwoPi else
  ang

def courseAngle(angle: Scalar): Scalar = // equiv angle in range [0, 2*Pi]
  val ang = angle % TwoPi
  if ang < 0 then ang + TwoPi else
  if ang > TwoPi then ang - TwoPi else
  ang

def midAngle(a1: Scalar, a2: Scalar): Scalar = a1 + normAngle(a2 - a1) / 2

def nearAngMult(angle: Scalar, step: Scalar): Scalar =
  // nearest multiple of an angular step
  val ang = if (angle < 0) angle + TwoPi else angle
  Int(ang / step + 0.5) * step

def nearMult(x: Scalar, y: Scalar=1): Scalar = // nearest multiple
  Int(x / y + (if (x > 0) 0.5 else -.5)) * y
def nearMultDif(x: Scalar, y: Scalar) = x - nearMult(x, y)
def nearMultDev(x: Scalar, y: Scalar) = abs(x - nearMult(x, y))
def prevMult(t: Scalar, dt: Scalar): Scalar = Int(t/dt) * dt

def rss(args: Scalar*): Scalar = rss(args.toList) // root sum square
def rss(args: List[Scalar]): Scalar = sqrt(args.foldLeft(zero)(_ + sqr(_)))
def sqr(s: Scalar): Scalar = s * s
def cube(s: Scalar): Scalar = s * s * s
def hypot(x: Scalar, y: Scalar): Scalar = sqrt((x*x)+(y*y))

def areClose(x: Scalar, y: Scalar) =
  if y == zero then x == zero else abs(x/y-1) < 1e-14

def bisection( // solve (invert) function using bisection and interpolation
  func: Scalar => Scalar, // function to be solved for
  target: Scalar, // target y value
  range: (Scalar, Scalar), // limiting x value for max delay
  tol: Scalar = 0, // solution tolerance
  nmax: Int = 8 // max number of bisection steps to perform
  ): Scalar =

  var (x0, x1) = range
  var y0 = zero
  var y1 = func(x1)
  var count = 0

  if y1 <= target then return x1

  while count < nmax do // bisection steps

    val xmid = (x0 + x1) / 2
    val ymid = func(xmid)

    if ymid < target then { x0 = xmid; y0 = ymid }
    else { x1 = xmid; y1 = ymid }

    //printlnx("mathx.scala bisection count =", count)
    if abs(ymid - target) < tol then count = nmax else count += 1

  if abs(y1 - y0) < 1e-3 * tol then y0 else
  x0 + (x1 - x0) * (target - y0) / (y1 - y0)
