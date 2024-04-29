import org.scalatest.funsuite.AnyFunSuite

import java.io.FileWriter

class TestMagic extends AnyFunSuite {


  test("Souffle vs implementation") {
    val size = 7
    val vars = 3
    val fields = 1

    repeat(10000, size, vars, fields)

  }


  test("slides") {
    val p = Parser.ParseTemplate("slides")
    val q = 5
    single(p, q)
  }

  test("QWE") {
    val p = Parser.ParseTemplate("qwe")
    val q = 1
    single(p, q)
  }



  test("Various exhaustive formulations") {
    val p = Parser.ParseTemplate("slides")
    val q = 5

    val standard = DatalogCompiler.datalogDir + "exhaustive.dl"
    val standardOut = DatalogCompiler.datalogDir + "demand.dl"
    val standardSol = "untitled/sol.tsv"

    single(p, q, standard, standardOut, standardSol)


    val alt1 = DatalogCompiler.datalogDir + "exhaustive1.dl"
    val alt1Out = DatalogCompiler.datalogDir + "demand1.dl"
    val alt1Sol = "untitled/sol1.tsv"

    single(p, q, alt1, alt1Out, alt1Sol)

  }


  private def single(p: Program, q: Cell, exhaustivePath: String, transformedOutput: String, solOutput: String): Unit = {
    DatalogCompiler.compileAndAnalyze(p, q, exhaustivePath, transformedOutput)
    DatalogCompiler.solutionToSingleTSV(solOutput)
  }


  private def single(p: Program, q: Cell): Unit = {

    DatalogCompiler.compileAndAnalyze(p, q)
    DatalogCompiler.solutionToSingleTSV("untitled/souffleSol.tsv")

    val solver = MagicSets()
    val solution = solver.solve(p, q)

    // Write the solution to the disk and read the contents of the files to compare pointsTo relations
    writeSolutionToDisk(solution)
    if !compareSouffleToMagic() then
      p.print()
      throw Error("Mismatch for program with query " + q)

    // Obtain demanded and tracked tokens to compare
    val souffleDemanded = DatalogCompiler.collectDemand()
    val souffleTracked = DatalogCompiler.collectTracked()

    val magicDemanded = solver.collectDemand
    val magicTracked = solver.collectTracked

    if souffleDemanded != magicDemanded then
      p.print()
      println("Difference in magic vs souffle demanded\nSouffle:")
      println(souffleDemanded)
      println("Magic:")
      println(magicDemanded)
      throw Error("Mismatch in demand for program with query " + q)

    if souffleTracked != magicTracked then
      p.print()
      println("Difference in magic vs souffle tracked\nSouffle:")
      println(souffleDemanded)
      println("Magic:")
      println(magicDemanded)
      assert(souffleTracked == magicTracked)
      throw Error("Mismatch in tracked for program with query " + q)
  }


  private def repeat(times: Int, size: Int, vars: Int, fields: Int): Unit = {
    for i <- 0 to times do
      val seed = scala.util.Random.nextInt()
      val g = ProgramGenerator(seed, vars, size, fields)
      val p = g.generate()
      val q = g.genQuery
      single(p, q)
      if i != 0 && i % 100 == 0 then println(s"Completed $i tests")

  }

  private def writeSolutionToDisk(sol: Solution, path: String = "untitled/magicSol.tsv"): Unit = {
    val builder = new StringBuilder()
    val writer = FileWriter(path)

    sol.foreach((cell, solution) => {
      solution.foreach(t => {
        cell match
          case a: Var => writer.write(s"x$a\tt$t\n")
          case b: (Token, Field) => writer.write(s"t${b._1}\tf${b._2}\tt$t\n")

      })
    })
    writer.close()
  }


  private def compareSouffleToMagic(soufflePath: String = "untitled/souffleSol.tsv", magicPath: String = "untitled/magicSol.tsv"): Boolean = {
    val souffle = FileManager.readFile(soufflePath).sorted
    val magic = FileManager.readFile(magicPath).sorted
    magic == souffle
  }

}
