package scalar.test

import tools_._

object CommandLineTest {

    def main(args: Array[String]) = {

        //for (arg <- args) println(arg)

        //val argString = "x= 123, 22, zz = blah, 43.5, y =true , gg, rr=3.445"
        //val args = argString.trim.split(" +")

        val options = Set("x", "y", "zz", "rr", "k", "a")

        val com = CommandLine(args, options)

        //println("short = " + com.shortBoolean("t"))

        val x = com.Int("x")
        val k = com.Int("k", -33) // -33 is the default value
        val rr = com.Real("rr")
        val zz = com.Text("zz")
        val a2 = com.Real(2)
        val y = com.Bool("y") // "short" Boolean ("t" or "f")
        val a = com.Bool("a")
        val a1 = com.Int(1)
        val a5 = com.Int(5, 22)

        println
        println("options: " + options)
        println
        println("x = " + x)
        println("k = " + k)
        println("rr = " + rr)
        println("y = " + y)
        println("a = " + a)
        println("zz = " + zz)
        println("a1 = " + a1)
        println("a2 = " + a2)
        println("a5 = " + a5)
        println

        println
        }
    }
