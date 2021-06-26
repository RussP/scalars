package types_ // type aliases, conversions, and other useful stuff

import language.implicitConversions

type Real = Double // better name for Double
type Bool = Boolean // shorter name for Boolean
type Text = String // better name for String

def not(b: Bool) = ! b // clearer form of not!
def and(a: Bool, b: => Bool) = a && b // I don't use this, but it's there
def or (a: Bool, b: => Bool) = a || b // ditto
def xor(a: Bool, b: Bool) = (a && !b) || (b && !a)

def Text(x: Any) = x.toString

def toInt (t: Text) = t.toInt // better form for conversion
def toLong(t: Text) = t.toLong // ditto
def toReal(t: Text) = t.toDouble // ditto
def toBool(t: Text) = t.toBoolean // ditto

def truncToInt (r: Real) = r.toInt // truncate real to integer
def truncToLong(r: Real) = r.toLong // truncate real to long integer

type Vec[T] = Vector[T] // shorthand name for Vector
type Opt[T] = Option[T] // shorthand name for Option
def Vec[T](x: T*) = Vector[T](x: _*)
def Opt[T](x: T) = Option[T](x: T)

def TextBuilder(t: Text="") = new StringBuilder(t) // better name!
implicit def StringBuilderToText(t: StringBuilder): Text = Text(t)

def printx(a: Any*)(implicit s: Text=" ") = print(a.mkString(s))
def printlnx(a: Any*)(implicit s: Text=" ") = println(a.mkString(s))

def splitLine(line: Text) = line.trim.split(" *[ ,] *").toVector
def recordFromLine(line: Text) = splitLine(line)

def stop = // breakpoint for debugging
  print("\npress <enter> to continue ")
  scala.io.StdIn.readLine()
  ()

def clockTime: Real = System.nanoTime/1e9 // time in seconds for profiling
