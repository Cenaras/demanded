import org.scalatest.funsuite.AnyFunSuite

class TestWavePropagation extends AnyFunSuite {


  test("small programs") {
    val vars = 3
    val fields = 1
    val insn = 7

    compare(vars, fields, insn)

  }


  private def compare(vars: Int, fields: Int, size: Int) = {

    for (i <- 0 until 1000000) {
      val seed = scala.util.Random.nextInt()
//      val seed = -749329879

      val g = new SimpleProgramGenerator(seed, vars, size, fields)
      val p = g.generate()
//      val p = Parser.ParseTemplate("qwe")

      val naive = NaiveExhaustiveSolver()
      val wp = WavePropagation()

      val naive_sol = naive.solve(p)
      val wp_sol = wp.solve(p)

      val same = TestUtil.compareExhaustiveToWP(naive_sol, wp_sol)
      if !same then
        println(s"Mismatch for program with seed ${seed}:")
        p.print()
        println("Exhaustive: ")
        println(naive_sol)
        println("Wave Wave Propagation: ")
        wp_sol.printSol()
        assert(false)

    }
  }


  // Some code duplication is always good!


}
