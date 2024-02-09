package main.program

import scala.collection.mutable.ArrayBuffer

object ProgramTemplates {

  /**
   *
   * x1 = new t1
   * x2 = new t2
   * x1 = x2
   * x3 = new t2
   * x1.f = x3
   * x4 = x1.f
   */
  def LoadStore: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(1, 1),
        NewInsn(2, 2),
        AssignInsn(1, 2),
        NewInsn(3, 2),
        StoreInsn(1, "f", 3),
        LoadInsn(4, 1, "f")
      ))
  }

  /**
   * // x = {f: {}}
   * x1 = new t1
   * x2 = new t2
   * x1.f = x2
   *
   * // z = x
   * x3 = x1
   *
   * // z.f = {}
   * x4 = new t3
   * x3.f = x4
   *
   * // y = x.f
   * x5 = x1.f
   */
  def Aliasing: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(1, 1),
        NewInsn(2, 2),
        StoreInsn(1, "f", 2),
        AssignInsn(3, 1),
        NewInsn(4, 3),
        StoreInsn(3, "f", 4),
        LoadInsn(5, 1, "f")
      ))
  }

  /**
   * x1 = new t1
   * x2 = new t2
   * x3 = new t3
   * x1 = x2
   */
  def demandedSimple: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(1, 1),
        NewInsn(2, 2),
        NewInsn(3, 3),
        AssignInsn(1, 2)
      ))
  }

  /**
   * This program previously gave us problems. Since we are reading from x3.f, we must track all tokens from x3, which
   * in this example initially will be t1. Then we must propagate t1 over to x4 and then from there to x2, such that
   * x2.f will actually perform the write (placing x1 in demand, then x2, then x0 which leads us to the t0 token)
   * This was implemented wrongly previously which caused an issue.
   *
   * x0 = new t0
   * x4 = new t1
   *
   * x0 = x4         x0 holds t0 t1
   * x2 = x0         x2 holds t0 t1
   * x1 = x2         x1 holds t0 t1
   *
   * x2.f = x1       t0.f and t1.f hold t0 and t1
   *
   * x3 = x4         x3 holds t1
   * x4 = x3.f       x4 holds t0, t1  which means x3 holds both due to above
   *
   */
  def TransitiveTokenTracking: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(0, 0),
        NewInsn(4, 1),
        AssignInsn(0, 4),
        AssignInsn(2, 0),
        AssignInsn(1, 2),
        StoreInsn(2, "f", 1),
        AssignInsn(3, 4),
        LoadInsn(4, 3, "f"),
      )
    )
  }

  /**
   * This small program gave us problems with query id 4, due to tracked tokens not being propagated unconditionally in 
   * a store operation. An error in our translation made it such that tracked tokens were only propagated if demand was
   * placed on the receiver constraint variable, which was wrong, as tracked tokens must always be propagated.
   *
   * x4 = new t1
   *  x5 = new t2
   *
   *  x4 = x3
   *  x3 = x4.f
   *
   *  x5.f = x4
   *  x1.f = x5
   *  x1 = x5.f
   */
  def UnconditionalTokenTrackingInStore: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(4, 1),
        NewInsn(5, 2),
        AssignInsn(4, 3),
        StoreInsn(5, "f", 4),
        StoreInsn(1, "f", 5),
        LoadInsn(3, 4, "f"),
        LoadInsn(1, 5, "f"),
      )
    )
  }


}
