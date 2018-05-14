package scalar.test

import scalar._
import ATMunits._

object ConfigReaderTest {

    def main(args: Array[String]) {

        //val config = new tools.ConfigReader("ConfigReader-test.in")
        val config = tools.ConfigReader("ConfigReader-test.in")

        val xxx = config.Int("xxx")
        val yyy = config.Int("yyy")

        //config.readFromSection("Velocity Filtering")

        val TrackFilterAlpha = config.Real("TrackFilterAlpha")
        //val TrackFilterAlpha = config.Real("TrackFilterAlpha", 0.5)
        val AltFilterAlpha = config.Real("AltFilterAlpha", 0.5)

        //config.readFromSection("Altitude Parameters")

        val CruiseAltTol = config.Scalar("CruiseAltTol", -1 * ft)
        val LevelRateLimit = config.Scalar("LevelRateLimit", -1 * fpm)

        //val CruiseAltTol = config.Scalar("CruiseAltTol")
        //val LevelRateLimit = config.Scalar("LevelRateLimit")

        println
        println(config)
        println
        println("TrackFilterAlpha = " + TrackFilterAlpha)
        println("AltFilterAlpha = " + AltFilterAlpha)
        println
        println("CruiseAltTol = " + CruiseAltTol/ft + " ft")
        println("LevelRateLimit = " + LevelRateLimit/fpm + " fpm")
        println
        println("test/xxx = " + xxx)
        println("test/yyy = " + yyy)
        println
        }
    }
