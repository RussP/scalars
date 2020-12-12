
package break_ { // implementation of "break" and "continue"

  import util.control.ControlThrowable // for efficiency

  class Continue extends ControlThrowable
  class Break    extends ControlThrowable
  }

package object break_ {

  def continues(op: => Unit) = try { op } catch { case e: Continue => }
  def breaks   (op: => Unit) = try { op } catch { case e: Break => }

  def continue = throw new Continue
  def break = throw new Break
  }

/***************************************************************************

// example usage:

import break_._

breaks { for (i <- 0 to 5) continues {

  if (i > 3) break
  println(i)
  if (i > 1) continue
  println(1000+i)
  }}

Note: be careful not to use "break" without also using "breaks," or
"continue" without also using "continues"! If you do, your program may
exit with no indication of why, and the error could be hard to interpret
or isolate. If you suspect this problem but don't know where in your
code it is occurring, comment out the two instances of "with
ControlThrowable" above to get a stack trace and find out where it is
happening. And don't forget to put "with ControlThrowable" back in when
you are done, or your code will be horribly inefficienct.

***************************************************************************/
