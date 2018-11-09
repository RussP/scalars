/*****************************************************************************
Copyright (c) 2009, Russell A. Paielli <http://RussP.us>
All rights reserved.

See license file for terms of license.

This file implements the SI (System Internationale) metric system of
units and many other commonly used non-metric units. A user guide is
available at <http://RussP.us/scalar.htm>. Send comments, corrections or
suggestions to Russ.Paielli@gmail.com.

The units defined in this file are based on public information from the
following websites:

http://physics.nist.gov/cuu/Units/units.html
http://www.bipm.org/en/si/
http://www.ex.ac.uk/cimt/dictunit/dictunit.htm
http://ts.nist.gov/ts/htdocs/230/235/appxc/appxc.htm
http://en.wikipedia.org/wiki/Category:Units_of_measure
*************************************************************************** */

import mathx_._
import scalar_._

package object units_ { // standard physical units

  val rad = base_unit("", "radian", "angle")

  // SI metric base units:

  val s   = base_unit("s", "second", "time")
  val m   = base_unit("m", "meter", "length")
  val kg  = base_unit("kg", "kilogram", "mass")
  val A   = base_unit("A", "ampere", "electric current")
  val K   = base_unit("K", "kelvin", "temperature")
  val mol = base_unit("mol", "mole", "amount of substance")
  val cd  = base_unit("cd", "candela", "luminous intensity")

  // Common scaled variations of base units:

  val ms = unit("ms", s/1000, "millisecond", "time")
  val us = unit("us", ms/1000, "microsecond", "time")
  val mm = unit("mm", m/1000, "millimeter", "length")
  val um = unit("um", mm/1000, "micrometer", "length")
  val cm = unit("cm", m/100, "centimeter", "length")
  val km = unit("km", 1000*m, "kilometer", "length")
  val g  = unit("g" , kg/1000, "gram", "mass")
  val mg = unit("mg", g/1000, "milligram", "mass")
  val ug = unit("ug", mg/1000, "microgram", "mass")
  val mA = unit("mA", A/1000, "milliamp", "electric current")
  val uA = unit("uA", mA/1000, "microamp", "electric current")

  // Derived units with special names and symbols:

  val sr  = unit("sr", rad, "steradian", "solid angle")
  val deg = unit("deg", rad * math.Pi/180, "degree", "plane angle")
  val Hz  = unit("Hz", 1/s, "hertz", "frequency")
  val N   = unit("N", kg*m/sqr(s), "newton", "force")
  val Pa  = unit("Pa", N/sqr(m), "pascal", "pressure")
  val J   = unit("J", N*m, "joule", "energy/work/heat")
  val W   = unit("W", J/s, "watt", "power")
  val C   = unit("C", A*s, "coulomb", "electric charge")
  val V   = unit("V", W/A, "volt", "electric potential")
  val F   = unit("F", C/V, "farad", "capacitance")
  val ohm = unit("ohm", V/A, "ohm", "electric resistance")
  val S   = unit("S", A/V, "siemens", "electric conductance")
  val Wb  = unit("Wb", V*s, "weber", "magnetic flux")
  val T   = unit("T", Wb/sqr(m), "tesla", "magnetic flux density")
  val H   = unit("H", Wb/A, "henry", "inductance")
  val lm  = unit("lm", cd*sr, "lumen", "luminous flux")
  val lx  = unit("lx", lm/sqr(m), "lux", "illuminance")
  val Bq  = unit("Bq", 1/s, "becquerel", "activity (of radionuclide)")
  val Gy  = unit("Gy", J/kg, "gray", "absorbed dose")
  val Sv  = unit("Sv", J/kg, "sievert", "dose equivalent")
  val kat = unit("kat", mol/s, "katal", "catalytic activity")

  // Common scaled variations of derived units:

  val kHz = unit("kHz", 1000*Hz, "kilohertz", "frequency")
  val MHz = unit("MHz", 1000*kHz, "megahertz", "frequency")
  val GHz = unit("GHz", 1000*MHz, "gigahertz", "frequency")
  val kN  = unit("kN", 1000*N, "kilonewton", "force")
  val kPa = unit("kPa", 1000*Pa, "kilopascal", "pressure")
  val kJ  = unit("kJ", 1000*J, "kilojoule", "energy/work/heat")
  val MJ  = unit("MJ", 1000*kJ, "megajoule", "energy/work/heat")
  val GJ  = unit("GJ", 1000*MJ, "gigajoule", "energy/work/heat")
  val mW  = unit("mW", W/1000, "milliwatt", "power")
  val kW  = unit("kW", 1000*W, "kilowatt", "power")
  val MW  = unit("MW", 1000*kW, "megawatt", "power")
  val GW  = unit("GW", 1000*MW, "gigawatt", "power")
  val kV  = unit("kV", 1000*V, "kilovolt", "electric potential")
  val mS  = unit("mS", S/1000, "millisiemens", "electric conductance")
  val mH  = unit("mH", H/1000, "millihenry", "inductance")
  val uH  = unit("uH", mH/1000, "microhenry", "inductance")
  val mF  = unit("mF", F/1000, "millifarad", "capacitance")
  val uF  = unit("uF", mF/1000, "microfarad", "capacitance")

  // other common units (including non-metric):

  val Min = unit("Min", 60*s, "minute", "time")
  val hr  = unit("hr", 60*Min, "hour", "time")

  val ft  = unit("ft", 0.3048*m, "feet", "length")
  val FL  = unit("FL", 100*ft, "Flight Level", "length") // aircraft altitude
  val kft = unit("kft", 1000*ft, "kilofeet", "length") // aircraft altitude
  val In  = unit("In", ft/12, "inch", "length")
  val nmi = unit("nmi", 1852*m, "nautical mile", "length")
  val mi  = unit("mi", 1609.344*m, "statute mile", "length")

  val lbm = unit("lbm", 0.45359237*kg, "pound", "mass")
  val lbf = unit("lbf", 4.448222*N, "pound", "force")
  val dyn = unit("dyn", 1e-5*N, "dyne", "force")
  val slug = unit("slug", 14.59390*kg, "slug", "mass")

  val kn  = unit("kn", nmi/hr, "knot", "speed")
  val kph = unit("kph", km/hr, "kilometer/hour", "speed")
  val mph = unit("mph", mi/hr, "mile per hour", "speed")
  val fpm = unit("fpm", ft/Min, "foot/minute", "speed") // altitude rate
  val fps = unit("fps", ft/s, "foot/second", "speed")

  val gacc = unit("gacc", 9.80665*m/sqr(s), "gravity accel")

  val bar = unit("bar", 100*kPa, "bar", "pressure")
  val atm = unit("atm", 101.325*kPa, "atmosphere", "pressure")
  val psi = unit("psi", lbf/sqr(In), "pounds/inch^2", "pressure")
  val inHg = unit("inHg", 3386.388*Pa, "inches of mercury", "pressure")

  val hp = unit("hp", 746*W, "electric horsepower", "power") // mech. hp=745.69987 W

  val Wh  = unit("Wh", W*hr, "watt-hour", "energy")
  val kWh = unit("kWh", 1000*Wh, "kilowatt-hour", "energy")
  val Btu = unit("Btu", 1055.056*J, "British thermal unit", "energy") // Intl Table
  val erg = unit("erg", 1e-7*J, "erg", "energy")
  val eV  = unit("eV", 1.602176e-19*J, "electron-volt", "energy")
  val cal = unit("cal", 4.185*J, "gram (small) calorie", "energy")

  val rpm = unit("rpm", 2*math.Pi*rad/s, "rev/minute", "angular velocity")
  val cfm = unit("cfm", cube(ft)/Min, "feet^3/minute", "flow rate")

  val acre = unit("acre", 43560*sqr(ft), "acre", "area")
  val ha = unit("ha", 1e4*sqr(m), "hectare", "area")

  val gal = unit("gal", 231*cube(In), "US gallon", "volume") //usually of liquid
  val L = unit("L", 1000*cube(cm), "liter", "volume") // usually of liquid

  val Oe = unit("Oe", 79.577*A/m, "oersted", "magnetic field strength")
  val fc = unit("fc", lm/sqr(ft), "footcandle", "illuminance") // light intensity

  // predefined output conversions:

  unit("m^2", sqr(m), "square meter", "area")
  unit("m^3", cube(m), "cubic meter", "volume")
  unit("m/s", m/s, "meter/second", "speed")
  unit("m/s^2", m/sqr(s), "meter/second^2", "acceleration")
  unit("1/m", 1/m, "reciprocal meter", "wave number")
  unit("kg/m^3", kg/cube(m), "kilogram/meter^3", "mass density")
  unit("m^3/kg", cube(m)/kg, "meter^3/kilogram", "specific volume")
  unit("A/m^2", A/sqr(m), "ampere/meter^2", "current density")
  unit("A/m", A/m, "ampere/meter", "magnetic field strength")
  unit("mol/m^3", mol/cube(m), "mole/meter^3", "substance concentration")
  unit("cd/m^2", cd/sqr(m), "candela/meter^2", "luminance")

  unit("Pa*s", Pa*s, "pascal seconds", "dynamic viscosity")
  unit("N*m", N*m, "newton meters", "moment of force")
  unit("N/m", N/m, "newton/meter", "surface tension")
  unit("rad/s", rad/s, "radian/sec", "angular velocity")
  unit("rad/s^2", rad/sqr(s), "radians/second^2", "angular acceleration")
  unit("W/m^2", W/sqr(m), "watt/meter^2", "heat flux density, irradiance")
  unit("J/K", J/K, "joule/kelvin", "heat capacity, entropy")
  unit("J/(kg*K)", J/(kg*K), "joule/(kg-kelvin)", "spec. heat capacity or entropy")
  unit("J/kg", J/kg, "joule/kilogram", "specific energy")
  unit("W/(m*K)", W/(m*K), "watt/kelvin", "thermal conductivity")
  unit("J/m^3", J/cube(m), "joule/meter^3", "energy density")
  unit("V/m", V/m, "volt/meter", "electric field strength")
  unit("C/m^3", C/cube(m), "coulomb/meter^3", "electric charge density")
  unit("C/m^2", C/sqr(m), "coulomb/meter^2", "electric flux density")
  unit("F/m", F/m, "farad/meter", "permittivity")
  unit("H/m", H/m, "henry/meter", "permeability")
  unit("J/mol", J/mol, "joule/mole", "molar energy")
  unit("J/(mol*K)", J/(mol*K), "joule/(mole-kelvin)", "molar entropy")
  unit("C/kg", C/kg, "coulomb/kiligram", "exposure to x and gamma rays")
  unit("Gy/s", Gy/s, "gray/second", "absorbed dose rate")
  unit("W/sr", W/sr, "watt/steradian", "radiant intensity")
  unit("W/(m^2*sr)", W/(sqr(m)*sr), "watt/(meter^2-steradian)", "radiance")
  unit("kat/m^3", kat/cube(m), "katal/meter^3", "catalytic concentration")

  unit("nmi/min", nmi/Min, "nautical mile/minute", "speed")
  unit("ft*lbf", ft * lbf, "foot-pound", "moment of force")

  unit("In^2", sqr(In))
  unit("In^3", cube(In))
  unit("ft^2", sqr(ft))
  unit("ft^3", cube(ft))
  unit("mi^2", sqr(mi))
  unit("mi^3", cube(mi))
  unit("nmi^2", sqr(nmi))
  unit("nmi^3", cube(nmi))

  val degK = K // alternative name for degrees Kelvin
  val degF = unit("degF", K/1.8, "degree Fahrenheit", "temperature")
  val degC = unit("degC", K, "degrees Celsius", "temperature")

  // absolute temperature conversions:
  // (note: 5/9 scaling for Fahrenheit done implicitly by degree unit)

  def convertTempKtoC(T: Scalar) = T - 273.15 * K // Kelvin to Celsius
  def convertTempCtoK(T: Scalar) = T + 273.15 * K // Celsius to Kelvin
  def convertTempFtoC(T: Scalar) = T - 32 * degF // Fahrenheit to Celsius
  def convertTempCtoF(T: Scalar) = T + 32 * degF // Celsius to Fahrenheit
  def convertTempFtoK(T: Scalar) = convertTempCtoK(convertTempFtoC(T))
  def convertTempKtoF(T: Scalar) = convertTempCtoF(convertTempKtoC(T))
  }
