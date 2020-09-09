
/***********************************************************************
This class implicitly extends the standard Scala Vector class to add
vector addition and other basic operations on algebraic vectors
********************************************************************** */

package scalar_

import scala.language.implicitConversions

object Vectorx {

implicit class Vectrx(self: Vector[Scalar]) {
  // adds basic mathematical operations to the Vector[Scalar] class

  import self._

  protected def check(v: Vector[Scalar]): Unit = { // check for compatibility

    if (length == v.length) return

    val txt = "\nincompatible Vectors lengths: "
    val msg = txt + length + ", " + v.length

    throw new RuntimeException(msg)
    }

  def x = self(0)
  def y = self(1)
  def z = self(2)

  def apply(i: Int): Scalar = self(i)
  def end(i: Int): Scalar = self(length - 1 + i)

  def unary_- = map(-_)

  protected def zip(v: Vector[Scalar]) = { check(v); self zip v }

  def + (v: Vector[Scalar]) = zip(v).map(t => t._1 + t._2)
  def - (v: Vector[Scalar]) = zip(v).map(t => t._1 - t._2)

  def * (s: Scalar) = map(_ * s)
  def / (s: Scalar) = map(_ / s)
  def % (s: Scalar) = map(_ % s)

  def sumx: Scalar = foldLeft(zero)(_ + _) // sum already in use for Vector
  def mean: Scalar = sumx / length
  def sumsqr: Scalar = foldLeft(zero)(_ + mathx_.sqr(_))
  def isZero: Boolean = forall(_ == 0)

  def mag: Scalar = sqrt(sumsqr) // magnitude
  def dir: Scalar = if (isZero) zero else atan2(x, y) // direction (course)

  def toUnitVector = if (isZero) self else self / mag

  def separation(that: Vector[Scalar]) = (that - self).mag
  def directionTo(that: Vector[Scalar]) = (that - self).dir
  def unitVectorTo(that: Vector[Scalar]) = (that - self).toUnitVector

  def dot(v: Vector[Scalar]): Scalar = // vector dot product
    zip(v).foldLeft(zero)((a,b) => a + b._1 * b._2)

  def cross(v: Vector[Scalar]) = Vector( // vector cross product
    y * v.z - z * v.y,
    z * v.x - x * v.z,
    x * v.y - y * v.x
    )
  }
}
