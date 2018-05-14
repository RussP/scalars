package scalar.test

import break._

object BreakTest {

    def main(args: Array[String]) {

        println

        breaks { for (i <- 0 to 5) continues {

            println
            if (i > 3) break
            println("i = " + i)
            if (i > 1) continue
            println("i = " + (1000+i))
            println

            breaks { for (j <- 0 to 5) continues {

                if (j > 3) break
                println("j = " + j)
                if (j > 1) continue
                println("j = " + (1000+j))
                }}
            }}

        println("completed successfully\n")
        }
    }
