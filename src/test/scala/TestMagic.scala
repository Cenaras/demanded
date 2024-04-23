import org.scalatest.funsuite.AnyFunSuite

import java.io.FileWriter

class TestMagic extends AnyFunSuite {


  test("Souffle vs implementation") {
    val seed = 1
    val size = 7
    val vars = 3
    val fields = 1


    val g = ProgramGenerator(seed, vars, size, fields)
    val p = g.generate()
    val q = g.genQuery
    p.print()

    println("Query " + q)
    DatalogCompiler.compileAndAnalyze(p, q)
    val demanded = DatalogCompiler.collectDemand()
    val tracked = DatalogCompiler.collectTracked()

    println(demanded)
    println(tracked)

    val solver = HeintzeTardieu()
    val solution = solver.solve(p, q)


    writeSolutionToDisk(solution)


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

    println("QWEQWE")

    sol.foreach((cell, solution) => {
      println(cell)
      println(solution)
    })

  }


}
