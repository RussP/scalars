
/****************************************************************************
This file implements the RingBuffer class, a basic circular buffer that
is used to queue input data for later use.
************************************************************************** */

package tools_

import types_._

case class RingBuffer[T: Manifest](size: Int): // basic ring buffer

  private var cell = new Array[T](size) // cells
  private var ptr = -1 // pointer to last cell written in
  private var nwrite = 0 // number of writes to ring buffer

  def apply(i: Int=0): T = // read data in cell

    val msg1 = "attempt to read ahead in ring buffer (i>0): "
    val msg2 = "attempt to read too far back in ring buffer: "

    if i > 0 then throw new RuntimeException(msg1 + i)
    if size + i < 1 then throw new RuntimeException(msg2 + i)

    val ptr0 = ptr + i
    val ptr1 = if (ptr0 < 0) ptr0 + size else ptr0

    cell(ptr1)

  override def toString: Text =

    val txt  = TextBuilder("RingBuffer:")
    txt ++= "\n  size = " + size
    txt ++= "\n  ptr = " + ptr
    txt ++= "\n  nwrite = " + nwrite
    for i <- 0 to size - 1 do txt ++= "\n    [" + i + "]: " + cell(i)
    txt

  def nStored = math.min(nwrite, size)

  def write(obj: T): Unit =
    ptr += 1
    if ptr >= size then ptr -= size
    cell(ptr) = obj
    nwrite += 1

  def replace(obj: T) = cell(ptr) = obj
