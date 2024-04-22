//import main.constraint.{ConstraintEnvironment, ConstraintGenerator, ConstraintVariables}
//import main.program.{Program, ProgramDistribution, ProgramGenerator, ProgramTemplates}
//import main.solver.HTSolver
//import main.util.{DatalogCompiler, FileManager, PrettyPrinter}
//import org.scalatest.funsuite.AnyFunSuite
//
// // UNUSED FOR NOW, THERE IS A DIFFERENCE WHEN LOAD/STORE IS MISMATCHED
//
///** Using the magic set transformation provides a demand driven logic program analysis.
// * The literature claims equivalence between Heintze-Tardieu and Magic sets transformations, without ever showing it.
// * Using the "exhaustive.dl" and "transform_program.sh" script, we can obtain a magic sets transformed analysis.
// *
// * We generate random programs (without function calls) and using the small compiler we produce datalog program files
// * and compare the results of the outputs.
// * */
//class TestHTvsMagic extends AnyFunSuite {
//
//  test("Small programs") {
//    for (i <- 0 until 100) {
//      val p: Program = TestUtil.newGenerator(4, 2, 10, TestUtil.newDist(25, 30, 20, 25)).generate()
//      assertEqual(p, p.getRandomVar)
//    }
//  }
//
//  test("MagicLoadNoStoreNonOptimal") {
//    assertEqual(ProgramTemplates.MagicLoadNoStoreNonOptimal, 0)
//  }
//
//
//  private def assertEqual(p: Program, query: Int): Unit = {
//    DatalogCompiler.compileAndAnalyze(p, query)
//
//    val constraints = ConstraintGenerator.generate(p)
//    val solver = new HTSolver()
//    val solution = solver.solve(constraints, query)
//
//    val datalogSolPath = "./untitled/datalogSolution.tsv"
//    val htSolutionPath = "./untitled/htSolution.tsv"
//
//    PrettyPrinter.outputTSV(solution, htSolutionPath)
//    DatalogCompiler.solutionToSingleTSV(datalogSolPath)
//
//    val equal = compare(solver, solution, htSolutionPath, datalogSolPath)
//    if (!equal) {
//      println("Program with query x" + query)
//      println(PrettyPrinter.stringifyProgram(p))
//    }
//    assert(equal)
//  }
//
//
//  /** *
//   * Compares the solution of the solver to the logic program result. Also compares the sets Q and W with the magic
//   * predicates to see differences/check for optimality.
//   *
//   * @param solver the solver
//   */
//  def compare(solver: HTSolver, solution: ConstraintVariables, htSolPath: String, datalogSolPath: String): Boolean = {
//    val q = solver.Q
//    val w = solver.W
//
//    val htSol = FileManager.readFile(htSolPath)
//    val datalogSol = FileManager.readFile(datalogSolPath)
//    if (!(htSol == datalogSol)) {
//      println("Solutions were different")
//      println("HT Solution")
//      println(htSol)
//      println("Magic solution")
//      println(datalogSol)
//
//      return false
//    }
//
//    // Assumes sorted newline tsv format
//    val datalogDemand = DatalogCompiler.collectDemand()
//    val htDemand = q.map(c => c.name.replace('.', '\t')).mkString("\n").linesIterator.toList.sorted.mkString("\n")
//    if (!(datalogDemand == htDemand)) {
//      println("Demand was different")
//      println("HT demand")
//      println(htDemand)
//      println("Magic demand")
//      println(datalogDemand)
//      return false
//    }
//
//    val datalogTracked = DatalogCompiler.collectTracked()
//    val htTracked = w.map(t => t.name).mkString("\n").linesIterator.toList.sorted.mkString("\n")
//    if (!(htTracked == datalogTracked)) {
//      println("Tracked tokens were different")
//      println("HT tracked")
//      println(htTracked)
//      println("Magic tracked")
//      println(datalogTracked)
//      return false
//    }
//
//    true
//  }
//
//
//}
//
