
import mathx._
import scalar._

package object ATMunits { // standard units for Air Traffic Management

  val rad = base_unit("", "radian", "angle") // dimensionless
  val sec = base_unit("sec", "second", "time")
  val ft  = base_unit("ft", "foot", "length")
  val lbm = base_unit("lbm", "pound", "mass")

  val deg = unit("deg", rad*math.Pi/180, "degree", "angle")

  val Min = unit("Min", 60*sec, "minute", "time")
  val hr  = unit("hr", 60*Min, "hour", "time")

  val FL  = unit("FL", 100*ft, "Flight Level", "length") // altitude
  val kft = unit("kft", 1000*ft, "kilofoot", "length") // altitude
  val nmi = unit("nmi", 6076.11549*ft, "nautical mile", "length")
  val km  = unit("km", nmi/1.852, "kilometer", "length")

  val kg  = unit("kg", lbm/0.45359237, "kilogram", "mass")

  val kn  = unit("kn", nmi/hr, "knot", "speed") // horizontal speed
  val fpm = unit("fpm", ft/Min, "feet/minute", "speed") // altitude rate

  val gacc = unit("gacc", 32.2*ft/sqr(sec), "gravity accel", "accel")

  val lbf  = unit("lbf", gacc*lbm, "pound", "force")

  //output_units("nmi", "Min")

  def timeFormat(time: Scalar): String = {

    val atime = abs(time)
    val hr1 = Int(atime / hr)
    val min1 = Int((atime % hr) / Min)
    val sec1 = atime % Min
    val sign = if (time < 0) "-" else ""

    if (hr1 == 0) "%s%d:%02.0f".form(sign, min1, sec1)
    else "%s%d:%02d:%02.0f".form(sign, hr1, min1, sec1)
    }

  }
