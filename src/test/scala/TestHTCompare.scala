import TestUtil.{newDist, newGenerator, repeatSolveBoth, solveBothDemanded}
import main.program.ProgramTemplates
import main.solver.SolverUtil.{compareSolutions, solutionSize}
import main.solver.{AMSolver, HTDouble, HTSolver, SolverUtil}
import main.util.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite

class TestHTCompare extends AnyFunSuite {


  test("Compare") {
    repeatSolveBoth(10000, newGenerator(20, 8, 60, newDist(20, 40, 20, 20)), solveBothDemanded, HTSolver(), HTDouble())
  }

  test("Compare with function call") {
    repeatSolveBoth(100000, newGenerator(8, 4, 12, newDist(15, 25, 10, 10, 20, 20)), solveBothDemanded, HTSolver(), HTDouble())
  }

  test("HT vs AM Function call") {
    repeatSolveBoth(100000, newGenerator(8, 4, 12, newDist(15, 25, 10, 10, 20, 20)), solveBothDemanded, HTSolver(), AMSolver())
  }

  test("Large compare") {
    repeatSolveBoth(1000, newGenerator(30, 10, 150, newDist(20, 40, 20, 20)), solveBothDemanded, HTSolver(), HTDouble())
  }

  test("MergeOfArguments") {
    val p = ProgramTemplates.MergeOfArguments
    val q = 1
    val sol = solveBothDemanded(p, q, HTSolver(), HTDouble())

    assert(compareSolutions(sol._1, sol._2, q))

    val sol1Size = SolverUtil.solutionSize(sol._1)
    val sol2Size = SolverUtil.solutionSize(sol._2)
    assert(sol1Size == sol2Size)
  }

  test("Multiple Fields") {
    val p = ProgramTemplates.MultipleFields
    val (one, two) = solveBothDemanded(p, 1, HTSolver(), HTDouble())
    assert(compareSolutions(one, two, 1))

    var sol = solveBothDemanded(p, (3, "f"), HTSolver(), HTDouble())
    assert(compareSolutions(sol._1, sol._2, (3, "f")))

    sol = solveBothDemanded(p, (3, "g"), HTSolver(), HTDouble())
    assert(compareSolutions(sol._1, sol._2, (3, "g")))
  }

  test("AM vs HT tracked return") {
    val p = ProgramTemplates.TrackedRetNodeImpliesTrackFunction
    val q = 0
    val sol = solveBothDemanded(p, q, HTSolver(), AMSolver())
    println("HT Solution")
    println(PrettyPrinter.stringifySolution(sol._1))
    println("AM Solution")
    println(PrettyPrinter.stringifySolution(sol._2))
    assert(compareSolutions(sol._1, sol._2, q))
  }

}
