package scalar.test

object CollectionSpeedTest2 {

    def main(args: Array[String]) {

        val option = 2
        val reps = 10000000

        val t0 = System.currentTimeMillis()

        if (option == 1) {

            var list = List[Int]()
            for (i <- 0 to reps) list ::= 0
            }

        else if (option == 2) {

            val listBuffer = scala.collection.mutable.ListBuffer[Int]()
            for (i <- 0 to reps) listBuffer.append(0)
            }

        else if (option == 3) {

            var vector = scala.collection.immutable.Vector[Int]()
            for (i <- 0 to reps) vector :+= i
            }

        val t1 = System.currentTimeMillis()
        val time = (t1 - t0) / 1000.0

        println("\n" + time + " seconds\n")
        }
    }
