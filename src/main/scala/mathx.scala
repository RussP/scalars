
package object mathx { // various math functions for Scalars

  import scalar._

  private type Real = Double

  val Pi = math.Pi
  val TwoPi = 2 * Pi
  val HalfPi = Pi / 2

  def scalarSteps(start: Scalar, end: Scalar, step: Scalar): Vector[Scalar] = {

    val inc = abs(step)
    val sgn = signum(step)

    (Real(start/inc) to Real(end/inc) by sgn).map(_ * inc).toVector
    }

  def scalarStepsx(start: Scalar, end: Scalar, step: Scalar): Vector[Scalar] = {
    // same as scalarSteps except guaranteed to include end point

    val steps = scalarSteps(start, end, step)

    if (areClose(steps.last, end)) steps else steps :+ end
    }

  def normAngle(angle: Scalar): Scalar = { // equivalent angle in range [-Pi,Pi]

    val ang = angle % TwoPi

    if (ang < -Pi) ang + TwoPi else
    if (ang >  Pi) ang - TwoPi else
    ang
    }

  def courseAngle(angle: Scalar): Scalar = { // equiv angle in range [0, 2*Pi]

    val ang = angle % TwoPi

    if (ang < 0) ang + TwoPi else
    if (ang > TwoPi) ang - TwoPi else
    ang
    }

  def midAngle(a1: Scalar, a2: Scalar): Scalar = a1 + normAngle(a2 - a1) / 2

  def nearAngMult(angle: Scalar, step: Scalar): Scalar = {
    // nearest multiple of an angular step

    val ang = if (angle < 0) angle + TwoPi else angle

    Int(ang / step + 0.5) * step
    }

  def nearMult(x: Scalar, y: Scalar=1): Scalar = // nearest multiple

    Int(x / y + (if (x > 0) 0.5 else -.5)) * y

  def nearMultDif(x: Scalar, y: Scalar) = x - nearMult(x, y)
  def nearMultDev(x: Scalar, y: Scalar) = abs(x - nearMult(x, y))

  def prevMult(t: Scalar, dt: Scalar): Scalar = Int(t/dt) * dt

  def rss(args: List[Scalar]): Scalar = // root sum square

    sqrt(args.foldLeft(zero)(_ + sqr(_)))

  def rss(args: Scalar*): Scalar = rss(args.toList) // root sum square
  def rms(args: Scalar*): Scalar = sqrt(rss(args.toList)) // root mean square
  def sqr(s: Scalar): Scalar = s * s
  def cube(s: Scalar): Scalar = s * s * s
  def hypot(x: Scalar, y: Scalar): Scalar = sqrt((x*x)+(y*y))

  def sin(x: Scalar): Real = math.sin(Real(x))
  def cos(x: Scalar): Real = math.cos(Real(x))
  def tan(x: Scalar): Real = math.tan(Real(x))

  def asin(x: Scalar): Real = math.asin(Real(x))
  def acos(x: Scalar): Real = math.acos(Real(x))
  def atan(x: Scalar): Real = math.atan(Real(x))

  def min(x: Scalar*): Scalar = x.min
  def max(x: Scalar*): Scalar = x.max

  //def min(x: Scalar, y: Scalar): Scalar = if (x < y) x else y
  //def max(x: Scalar, y: Scalar): Scalar = if (x > y) x else y

  def areClose(x: Scalar, y: Scalar) =
    if (y == 0) x == 0 else abs(x / y - 1) < 1e-13

  }
