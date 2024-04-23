import org.scalatest.funsuite.AnyFunSuite

import java.io.FileWriter

class TestMagic extends AnyFunSuite {


  test("Souffle vs implementation") {
    val size = 7
    val vars = 3
    val fields = 1

    repeat(100, size, vars, fields)

  }

  test("QWE") {
    val p = Parser.ParseTemplate("qwe")
    val q = 2


    val ht = HeintzeTardieu()
    ht.solve(p, q)
    ht.printSolution()


//    single(p, q)
  }


  private def single(p: Program, q: Cell): Unit = {

    DatalogCompiler.compileAndAnalyze(p, q)
    DatalogCompiler.solutionToSingleTSV("untitled/souffleSol.tsv")

    val solver = MagicSets()
    val solution = solver.solve(p, q)

    writeSolutionToDisk(solution)
    if !compareSouffleToMagic() then
      p.print()
      throw Error("Mismatch for program with query " + q)
  }


  private def repeat(times: Int, size: Int, vars: Int, fields: Int): Unit = {
    for i <- 0 to times do
      val seed = scala.util.Random.nextInt()
      val g = ProgramGenerator(seed, vars, size, fields)
      val p = g.generate()
      val q = g.genQuery
      single(p, q)

  }


  private def stringifyDemand(): String = {
    ""
  }

  private def stringifyTracked(): String = {
    ""
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
