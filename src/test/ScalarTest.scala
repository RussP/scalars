package scalar.test

import types_._
import tools_._
import mathx_._
import scalar_._
import ATMunits_._

case object ScalarTest {

  def main(args: Array[Text]) = {

    val tol = 1e-13 // numerical error tolerance

    println()

    printlnx( sin(30*deg) - 0.5 )
    printlnx( 10 * deg == Pi / 18 )
    printlnx( "true =", areClose(10 * deg, Pi / 18) )
    printlnx( "true =", areClose(sin(30*deg), 0.5) )

    val dist = 4 * nmi
    printlnx(dist/nmi)
    printlnx("4 nmi =", format(dist, "nmi"))
    printlnx("4 nmi =", format(dist, "nmi", "%6.3f"))
    printlnx("4 nmi =", "%6.3f".form(dist/nmi))

    output_units("nmi", "Min")
    output_units("nmi", "lbf", "kn", "deg")

    printlnx( 2 * kft / kft - 2, "= 0" )
    printlnx( 3 * nmi, "= 3 nmi" )
    printlnx( -nmi, "= -1 nmi" )
    printlnx( 30 * sec, "= 0.5 Min" )
    printlnx( 3.3 * kn, "= 3.3 kn" )
    printlnx( "utype(nmi) =", utype(3*nmi) )
    printlnx( "utype(pow(nmi,55)) =", utype(pow(nmi,55)) )
    printlnx( "sin(30 * deg) =", sin(30 * deg) )
    printlnx( "cos(60 * deg) =", cos(60 * deg) )
    printlnx( "tan(45 * deg) =", tan(45 * deg) )
    printlnx( "atan2(3 * nmi, 3 * nmi) =", atan2(3 * nmi, 3 * nmi) )
    printlnx( "normAngle(30*deg) =", normAngle(30*deg) )

    assert( 3 * nmi + 2 * nmi == 5 * nmi )
    println(300 * nmi / (1.5 * hr))
    println(200 * kn)
    assert( 300 * nmi / (1.5 * hr) == 200 * kn )
    assert( sqr(4 * sec) / sec == 16 * sec )
    assert( abs(-3 * sec) == 3 * sec )

    assert( 100 * kn * hr == 100 * nmi )
    assert( 1200 * ft / Min == 1200 * fpm )
    assert( abs(2 + 4 * ft / ft - 6) < tol )
    assert( -3 * kn < 0 )

    assert( min(2 * ft, 3 * ft) == 2 * ft )
    assert( max(2 * gacc, 3 * gacc) == 3 * gacc )

    assert( abs(sin(30*deg) - 0.5) < tol )
    assert( abs(cos(60*deg) - 0.5) < tol )
    assert( abs(tan(45*deg) - 1) < tol )

    assert( abs(atan2( 3 * nmi,  3 * nmi) -  45 * deg) < tol )
    assert( abs(atan2( 3 * nmi, -3 * nmi) - 135 * deg) < tol )
    assert( abs(atan2(-3 * nmi,  3 * nmi) +  45 * deg) < tol )
    assert( abs(atan2(-3 * nmi, -3 * nmi) + 135 * deg) < tol )

    assert( 6.0 * nmi / nmi == 6 )
    assert( 6 * nmi / nmi == 6 )

    assert( 0 * ft == 0 * sec)
    assert( 0 * ft == zero )
    assert( 4 * rad == 4 )
    assert( 4 * rad != 5 )
    assert( 3 * ft/ft == 3 )
    assert( 0 * ft == 0 )
    assert( 0 * ft != 1 )

    //assert( new Scalar(9) == 9 ) // works only with scalar class on
    //assert( new Scalar(9) != 7 ) // ditto
    //assert( 0 == 0 * ft ) // compile error (at least it's safe!)
    //assert( 3 == 3 * ft ) // compile error (at least it's safe!)

    try { 3 * ft == 3 }
    catch { case ex: RuntimeException => println("ERROR: ft == Int") }

    try { val y = 3 * ft + 2 * sec }
    catch { case ex: RuntimeException => println("ERROR: ft + sec") }

    //try { 6 * sec == "hello" }
    //catch { case ex: RuntimeException =>
    //    println("ERROR: cannot do 6 * sec == \"hello\"") }

    val z = List(4*ft, 3*ft, 7.5*ft, -2.9*ft)
    assert( z.max ==  7.5 * ft )
    assert( z.min == -2.9 * ft )

    println(unitList)
    println
    }}  

