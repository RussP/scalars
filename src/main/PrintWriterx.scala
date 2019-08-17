package tools_

import types_._

case class PrintWriterx(fileName: Text=nullFile)
  extends java.io.PrintWriter(fileName) {
  // adds convenience features to Java PrintWriter class

  val isNull = fileName == nullFile
  val notNull = not(isNull)

  override def print(text: Text) = if (notNull) super.print(text)
  override def println(text: Text) = if (notNull) super.println(text)

  def printx(a: Any*)(implicit s: Text=" ") = print(a.mkString(s))
  def printlnx(a: Any*)(implicit s: Text=" ") = println(a.mkString(s))
  }
