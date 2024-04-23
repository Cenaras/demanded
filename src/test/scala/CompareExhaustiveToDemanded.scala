import org.scalatest.funsuite.AnyFunSuite

class CompareExhaustiveToDemanded extends AnyFunSuite {

  test("small programs") {
    val vars = 3
    val fields = 1
    val insn = 7

    compareExhaustiveToDemanded(100000, insn, vars, fields, SolverType.HT)
  }
  test("medium programs") {
    val vars = 5
    val fields = 2
    val insn = 12

    compareExhaustiveToDemanded(100000, insn, vars, fields, SolverType.HT)
  }
  test("large programs") {
    val vars = 5
    val fields = 2
    val insn = 25

    compareExhaustiveToDemanded(100000, insn, vars, fields, SolverType.HT)
  }

  test("a million") {
    val vars = 4
    val fields = 2
    val insn = 15

    compareExhaustiveToDemanded(1000000, insn, vars, fields, SolverType.HT)
  }

  test("small magic") {
    val vars = 3
    val fields = 1
    val insn = 7

    compareExhaustiveToDemanded(1000000, insn, vars, fields, SolverType.Magic)
  }
  
  test("large magic") {
    val vars = 5
    val fields = 2
    val insn = 25

    compareExhaustiveToDemanded(10000, insn, vars, fields, SolverType.HT)
  }
  
  private def compareExhaustiveToDemanded(times: Int, size: Int, vars: Int, fields: Int, st: SolverType): Unit = {
    for i <- 0 to times do
      val seed = scala.util.Random.nextInt()
      val g = new ProgramGenerator(seed, vars, size, fields)
      val p = g.generate()
      val query = g.genQuery

      val ex = NaiveExhaustiveSolver()
      val dem = demandedSolver(st)
      val solEx = ex.solve(p)
      val solDem = dem.solve(p, query)

      if !TestUtil.compareSolutions(solEx, solDem, query) then
        println("Solution mismatch")
        p.print()
        ex.printSolution()
        dem.printSolution()
        throw new Error()
  }


  private enum SolverType:
    case HT, Magic


  private def demandedSolver(st: SolverType): DemandedSolver = {
    st match
      case SolverType.HT => HeintzeTardieu()
      case SolverType.Magic => MagicSets()
  }

}