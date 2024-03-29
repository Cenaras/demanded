import main.constraint.{ConstraintGenerator, ConstraintVariables}
import main.program.{Program, ProgramDistribution, ProgramGenerator}
import main.solver.*
import main.solver.SolverUtil.compareSolutions
import main.util.PrettyPrinter

object TestUtil {
  def solveDemanded(p: Program, queryId: QueryID, solver: Demanded): ConstraintVariables = {
    val constraints = ConstraintGenerator.generate(p)
    solver.solve(constraints, queryId)
  }

  def repeatTest(times: Int, generator: ProgramGenerator): Unit = {
    for (i <- 0 until times) {
      val program = generator.generate()
      val query = program.getRandomVar

      val (exhaustiveSolution, demandedSolution) = solveBoth(program, query)
      if (!compareSolutions(exhaustiveSolution, demandedSolution, query)) {
        throw Error("Solutions did not match with query %d for program\n%s".format(query, PrettyPrinter.stringifyProgram(program)))
      }

      val demandedSolutionSize = SolverUtil.solutionSize(demandedSolution)
      val exhaustiveSolutionSize = SolverUtil.solutionSize(exhaustiveSolution)
      assert(demandedSolutionSize <= exhaustiveSolutionSize)
    }
  }


  def repeatSolveBoth(times: Int,
                      generator: ProgramGenerator,
                      solve: (Program, QueryID, Demanded, Demanded) => (ConstraintVariables, ConstraintVariables),
                      sol1: Demanded,
                      sol2: Demanded): Unit = {

    for (i <- 0 until times) {
      val program = generator.generate()
      val query = program.getRandomVar

      val (solution1, solution2) = solve(program, query, sol1, sol2)
      if (!compareSolutions(solution1, solution2, query)) {
        throw Error("Solutions did not match with query %d for program\n%s".format(query, PrettyPrinter.stringifyProgram(program)))
      }
      val sol1Size = SolverUtil.solutionSize(solution1)
      val sol2Size = SolverUtil.solutionSize(solution2)
      if (sol1Size != sol2Size) {
        println("Solution 1:")
        println(PrettyPrinter.stringifySolution(solution1))
        println("Solution 2:")
        println(PrettyPrinter.stringifySolution(solution2))
        println("Program (query " + query + "): ")
        println(PrettyPrinter.stringifyProgram(program))
        assert(false)
      }
    }
  }

  def newGenerator(varNumber: Int, tokenNum: Int, size: Int, dist: ProgramDistribution): ProgramGenerator = {
    new ProgramGenerator(varNumber, tokenNum, size, dist)
  }

  def newDist(newObj: Int, assign: Int, load: Int, store: Int): ProgramDistribution = {
    newDist(newObj, assign, load, store, 0, 0)
  }

  def newDist(newObj: Int, assign: Int, load: Int, store: Int, newFun: Int, call: Int): ProgramDistribution = {
    new ProgramDistribution(newObj, assign, load, store, newFun, call)
  }

  /**
   * Solves two instances of a program (TODO: Allow us to specify solvers here)
   */
  def solveBoth(p: Program, queryId: QueryID): (ConstraintVariables, ConstraintVariables) = {
    val eConstraints = ConstraintGenerator.generate(p)
    val dConstraints = ConstraintGenerator.generate(p)

    val exhaustive = ExhaustiveSolver()
    val ht = new HTSolver()

    val exhaustiveSolution = exhaustive.solve(eConstraints)
    val demandedSolution = ht.solve(dConstraints, queryId)

    //    println(PrettyPrinter.stringifySolution(exhaustiveSolution))
    //    println(PrettyPrinter.stringifySolution(demandedSolution))

    (exhaustiveSolution, demandedSolution)
  }

  def solveBothDemanded(p: Program, queryID: QueryID, sol1: Demanded, sol2: Demanded): (ConstraintVariables, ConstraintVariables) = {
    val constraints1 = ConstraintGenerator.generate(p)
    val constraints2 = ConstraintGenerator.generate(p)

    val solution1 = sol1.solve(constraints1, queryID)
    val solution2 = sol2.solve(constraints2, queryID)
    (solution1, solution2)
  }


  def compareOptimality(p: Program, queryID: QueryID, sol1: Demanded, sol2: Demanded): ((Int, Int, Int), (Int, Int, Int)) = {
    val c1 = ConstraintGenerator.generate(p)
    val c2 = ConstraintGenerator.generate(p)

    val s1 = sol1.solve(c1, queryID)
    val s2 = sol2.solve(c2, queryID)

    val res1 = (sol1.Q.size, sol1.W.size, SolverUtil.solutionSize(s1))
    val res2 = (sol2.Q.size, sol2.W.size, SolverUtil.solutionSize(s2))

    (res1, res2)

  }


}
