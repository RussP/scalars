package scalar.test

import types_._
import tools_._
import scalar_._
import ATMunits_._

import System.currentTimeMillis
import scala.collection.immutable.{Vector => SVector}

object CollectionSpeedTest {

    def main(args: Array[String]) = {

        val com = new CommandLine(args)

        val n = com.Int("n", 100)
        val reps = com.Real(1, 1e8).toInt

        //val reps = if (args.length > 0) args(0).toDouble.toInt else 1e8.toInt

        printlnx("\n" + "n = " + n)
        printlnx(reps.toDouble, " repetitions\n")

        val list = Range(0, n).toList
        val array = Range(0, n).toArray

        //val vec = new VectorBuilder[Int]
        //for (i <- Range(0, n)) vec += i
        //val vector = vec.result

        var t0 = 0L
        var t1 = 0L
        var time = 0.0
        val n1 = n - 1

        //-------------------------------------------------
        /*
        t0 = currentTimeMillis()

        for (i <- 0 to reps) list(n1)

        t1 = currentTimeMillis()

        time = (t1 - t0) / 1000.0

        println("List:  " + time + " seconds")
        */
        //-------------------------------------------------

        t0 = currentTimeMillis()

        for (i <- 0 to reps) array(n1)

        t1 = currentTimeMillis()

        time = (t1 - t0) / 1000.0

        println("Array: " + time + " seconds")

        //-------------------------------------------------

        t0 = currentTimeMillis()

        var vector = SVector[Int]()
        for (i <- Range(0, n)) vector :+= i

        t1 = currentTimeMillis()

        time = (t1 - t0) / 1000.0

        println("Vector build: " + time + " seconds")

        //-------------------------------------------------

        t0 = currentTimeMillis()

        for (i <- 0 to reps) vector(n1)

        t1 = currentTimeMillis()

        time = (t1 - t0) / 1000.0

        println("Vector: " + time + " seconds")

        println
        }
    }
