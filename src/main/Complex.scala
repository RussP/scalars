package scalar_

import types_._
import mathx_._

import scala.language.implicitConversions

case class Complex(real: Scalar, imag: Scalar=0) {

  override def toString: Text =
    s"%g${if (imag<0) "-" else "+"}%gi".form(real, abs(imag))

  lazy val mag = hypot(real, imag) // magnitude
  lazy val dir = atan2(imag, real) // direction

  def abs_imag = abs(imag)

  def + (c: Complex) = Complex(real + c.real, imag + c.imag)
  def + (s: Scalar) = Complex(real + s, imag)

  def - (c: Complex) = Complex(real - c.real, imag - c.imag)
  def - (s: Scalar) = Complex(real - s, imag)

  def * (c: Complex) = Complex(real * c.real - imag * c.imag,
      real * c.imag + imag * c.real)
  def * (s: Scalar) = Complex(s * real, s * imag)

  def / (c: Complex): Complex = {
    val denom = sqr(c.real) + sqr(c.imag)
    val r1 = real * c.real + imag * c.imag
    val i1 = imag * c.real - real * c.imag
    Complex(r1, i1) / denom
    }

  def / (s: Scalar) = Complex(real / s, imag / s)

  def unary_- = Complex(-real, -imag)
  def conjugate = Complex(real, -imag)

  def pow(exp: Real): Complex = {
    val mag1 = scalar_.pow(mag, exp)
    val dir1 = exp * dir
    Complex(cos(dir1), sin(dir1)) * mag1
    }

  override def equals(that: Any): Bool = that match {
    case c: Complex => real == c.real && imag == c.imag
    case s: Scalar => real == s && imag == 0
    case r: Real => real == r && imag == 0 // ignore compiler warning
    case i: Int => real == i && imag == 0
    case _ => false
    }

  def ==: (a: Any) = equals(a) // allows reverse order of args for == test

  def toScalar: Scalar = {
    if (imag == 0) return real
    throw new RuntimeException(s"Cannot convert $this to Scalar")
    }

  def toReal = Real(toScalar)
  }

object Complex {

  def sqr(a: Complex) = a * a
  def cube(a: Complex) = a * a * a

  def sqrt(a: Complex): Complex = a.pow(0.5)
  def sqrt(a: Real): Complex = sqrt(Complex(a))
  def sqrt(a: Int): Complex = sqrt(Complex(a))

  def cbrt(a: Complex): Complex = a.pow(1/3.0)
  def cbrt(a: Real): Complex = cbrt(Complex(a))
  def cbrt(a: Int): Complex = cbrt(Complex(a))

  def pow(c: Complex, exp: Real): Complex = c.pow(exp)

  def toScalar(c: Complex) = c.toScalar
  def toReal(c: Complex) = c.toReal

  def ==: (a: Any, b: Complex) = a match {
    case a: Complex => a == b
    case a: Scalar => Complex(a) == b
    case a: Int => Complex(a) == b
    case _ => false
    }

  implicit def ScalarToComplex(r: Scalar): Complex = Complex(r)

  def quadraticRoots(a: Scalar, b: Scalar, c: Scalar): Vector[Complex] = {
    // roots of a quadratic polynomial ax^2 + bx + c = 0
    if (a == 0) return Vector(-c/b)
    val d = Complex.sqrt(b*b - 4*a*c)
    val x1 = (-b + d) / a / 2
    val x2 = (-b - d) / a / 2
    Vector(x1, x2)
    }

  def cubicRoots(a: Scalar, b: Scalar, c: Scalar, d: Scalar): Vector[Complex] = {
    // roots of a cubic polynomial ax^3 + bx^2 + cx + d = 0 based on
    // https://en.wikipedia.org/wiki/Cubic_equation#General_cubic_formula

    if (a == 0) return quadraticRoots(b, c, d)

    val d0 = b*b - 3*a*c
    val d1 = 2*b*b*b - 9*a*b*c + 27*a*a*d

    val g1 = (Complex.sqrt(-3) - 1) / 2
    val z1 = Complex.sqrt(d1*d1 - 4*d0*d0*d0)
    val f1 = d1 + z1
    val h1 = if (f1 != 0) f1 else d1 - z1

    val C1 = Complex.cbrt(h1/2)
    val C2 = C1 * g1
    val C3 = C2 * g1

    val x1 = -(b + C1 + d0/C1) / a / 3
    val x2 = -(b + C2 + d0/C2) / a / 3
    val x3 = -(b + C3 + d0/C3) / a / 3

    val Vector(r1, r2, r3) = Vector(x1, x2, x3).sortBy(_.abs_imag)
    Vector(r1.copy(imag=0), r2, r3) // avoid roundoff error for known real root
    }

  // The following trick avoids conflicts with Scalar implicit conversions:
  trait implicits1 { implicit def RealToComplex(r: Real) = Complex(r) }
  object implicits extends implicits1
  // to activate this implicit conversion, add the following line:
  // import Complex.implicits._
  }
