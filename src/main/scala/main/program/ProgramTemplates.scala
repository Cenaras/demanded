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
    Parser.ParseProgram(readTemplate("LoadStore"))
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
    Parser.ParseProgram(readTemplate("Aliasing"))
  }

  /**
   * x1 = new t1
   * x2 = new t2
   * x3 = new t3
   * x1 = x2
   */
  def DemandedSimple: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(1, 1),
        NewInsn(2, 2),
        NewInsn(3, 3),
        AssignInsn(1, 2)
      ))
  }


  def FunCall: Program = {
    Parser.ParseProgram(readTemplate("FunCall"))
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
    Parser.ParseProgram(readTemplate("TransitiveTokenTracking"))
  }

  /**
   * This small program gave us problems with query id 4, due to tracked tokens not being propagated unconditionally in
   * a store operation. An error in our translation made it such that tracked tokens were only propagated if demand was
   * placed on the receiver constraint variable, which was wrong, as tracked tokens must always be propagated. (That is,
   * the tracking was placed behind the implication x in Q => ... which is wrong).
   *
   * x4 = new t1
   * x5 = new t2
   * x4 = x3
   * x3 = x4.f
   * x5.f = x4
   * x1.f = x5
   * x1 = x5.f
   */
  def UnconditionalTokenTrackingInStore: Program = {
    Parser.ParseProgram(readTemplate("UnconditionalTokenTrackingInStore"))
  }

  /**
   * This program showcases the importance of always tracking the entire points-to set of a base variable in a store
   * operation. Intuitively, if we perform a store operation x.f = y, then we must track all tokens in x, since alias
   * relations might mean another (possibly non-demanded) operation may alter the solution of our demanded variable
   *
   * For this particular program with query x6, when processing x1.f = x3, we must track t1 since otherwise we never
   * realize that x5 holds t1, which means x0 = x5.f does not transfer ⟦t1.f⟧ to x0 (i.e., t2) and thereby we miss from
   * x0.f = x1 and x6 = x3.f that x6 holds t1 (which we determine in an exhaustive setting).
   *
   * I believe it suffices to track the tokens of x, only if y contains tracked tokens - thus we can restrict the
   * tracking of x, to only happen if (y n W) is non-empty, which is more restrictive than always tracking all tokens...
   *
   * x1 = new t1
   * x5 = new t1
   * x3 = new t2
   * x0.f = x1
   * x6 = x3.f
   * x0 = x5.f
   * x1.f = x3
   *
   * @return
   */
  def TrackBaseInStore: Program = {
    Parser.ParseProgram(readTemplate("TrackBaseInStore"))
  }



  /*
   Merge in arguments
    x0 holds t1
    x1 holds f2
    the function calls to x1 merges x0 and x5 in x2
    Thus both methods return t1, meaning that x4 should hold t1
  
x0 = new t1
x1 = (x2) =>_f2 x2
x3 = x1(x0)
x4 = x1(x5)

   */


  def MergeInCall: Program = {
    Parser.ParseProgram(readTemplate("MergeInCall"))
  }


  def MultipleFields: Program = {
    Parser.ParseProgram(readTemplate("MultipleFields"))
  }

  def FunctionAndField: Program = {
    Parser.ParseProgram(readTemplate("FunctionAndField"))
  }

  /**
   * Translates to the following program (we query x3). All function calls are identity functions!
   *
   * f = (a) => a
   * g = (b) => b
   * x2 = f(g)
   * x3 = x2(f)
   * x4 = x3(f)
   */
  def DoubleSingle: Program = {
    Parser.ParseProgram(readTemplate("DoubleSingle"))
  }

  /*
    The call x2 = f(g) calls function f
      a contains g

    The call x3 = x2(f) calls function g
      b contains f

    The call x4 = x3(f) calls function f
      a contains f


    Whenever f is called, the return will be f, g
    Whenever g is called, the return will be f

   */






  /**
   * Reads a template from the templates folder
   *
   * @param name filename from template folder.
   * @return contents of filename
   */
  private def readTemplate(name: String): String = {
    val source = scala.io.Source.fromFile("src/main/scala/main/program/templates/" + name)
    val lines = try source.getLines().map(_.trim).mkString("\n") finally source.close()
    lines
  }
}
