import org.scalatest.funsuite.AnyFunSuite

class CompareExhaustiveToHT extends AnyFunSuite {

  test("small programs") {
    val vars = 3
    val fields = 1
    val insn = 7

    repeatSolveAndCompare(100000, insn, vars, fields)

  }
  test("medium programs") {
    val vars = 5
    val fields = 2
    val insn = 12

    repeatSolveAndCompare(100000, insn, vars, fields)
  }
  test("large programs") {
    val vars = 5
    val fields = 2
    val insn = 25

    repeatSolveAndCompare(100000, insn, vars, fields)
  }

  def repeatSolveAndCompare(times: Int, size: Int, vars: Int, fields: Int): Unit = {
    for i <- 0 to times do
      val seed = scala.util.Random.nextInt()
      val g = new ProgramGenerator(seed, vars, size, fields)
      val p = g.generate()
      val query = g.genQuery

      val ex = NaiveExhaustiveSolver()
      val ht = HeintzeTardieu()
      val solEx = ex.solve(p)
      val solHt = ht.solve(p, query)

      if !TestUtil.compareSolutions(solEx, solHt, query) then
        println("Solution mismatch")
        p.print()
        ex.printSolution()
        ht.printSolution()
        throw new Error()
  }


}
