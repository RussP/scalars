package scalar.test

import scalar._
import units._

//import metascala.Units._

import System.currentTimeMillis

object ScalarSpeedTest {

    var yy = 0.0004 * m
    var zz = 0.0005 * m
    var jj = 0 * m * m

    def main(args: Array[String]) {

        val num = if (args.length > 0) args(0).toDouble.toLong else 1e8.toLong

        println("\n" + num.toDouble + " repetitions")

        val t0 = currentTimeMillis()

        var i = 0L
        var j = 0.01

        while (i < num) {

            i += 1
            j += 0.0001

            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)

            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)

            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)

            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            jj += yy * (j * yy + zz)
            }

        println(jj)

        val t1 = currentTimeMillis()

        val time = (t1 - t0) / 1000.0

        println(time + " seconds\n")
        }
    }
