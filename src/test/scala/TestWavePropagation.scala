import org.scalatest.funsuite.AnyFunSuite

class TestWavePropagation extends AnyFunSuite {


  test("small programs") {
    val vars = 3
    val fields = 1
    val insn = 7

    compare(vars, fields, insn)

  }


  test("Tidal") {
    for _ <- 0 until 100000 do {
      testTidal()
    }
  }


  private def testTidal() = {
    val seed = scala.util.Random.nextInt()
    val g = new SimpleProgramGenerator(seed, 3, 7, 1)
    val p = g.generate()
    //      val p = Parser.ParseTemplate("qwe")
    val q = g.genQuery


    val naive = NaiveExhaustiveSolver()
    val tidal = Tidal()

    val naive_sol = naive.solve(p)(q)
    val tidal_sol = tidal.solve(p, q)(q)

    if naive_sol != tidal_sol then

      println(s"Program with query ${q} and seed ${seed}")
      p.print()

      println("Exhaustive: ")
      println(naive_sol)
      println("Tidal Propagation: ")
      println(tidal_sol)

      assert(false)
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
