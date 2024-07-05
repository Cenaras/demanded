import org.scalatest.funsuite.AnyFunSuite

class MagicVariants extends AnyFunSuite {


  test("qwe") {

    for _ <- 0 until 100000 do {
      val seed = scala.util.Random.nextInt()
      val g = new SimpleProgramGenerator(seed, 5, 15, 2)
      val p = g.generate()
      val q = g.genQuery

      val ex = new NaiveExhaustiveSolver()
      val m0 = new MagicSet0()

      val exSol = ex.solve(p)
      val m0Sol = m0.solve(p, q)

      if !(exSol.get(q) == m0Sol.get(q)) then
        println(s"Mismatch in solutions for program with query ${q} and seed ${seed}:")
        p.print()
        println("ExSol:")
        println(exSol)
        println("m1:")
        println(m0Sol)
        throw Error()
    }

  }

  test("temp") {
    val seed = -1125504039
    val g = new SimpleProgramGenerator(seed, 3, 5, 1)
    val p = g.generate()
    val q = 0
    p.print()

    val ex = new NaiveExhaustiveSolver()
    val m0 = new MagicSet0()

    val exSol = ex.solve(p)
    val m0Sol = m0.solve(p, q)

    println(exSol)
    println(m0Sol)

//    val standard = new Standard()
//    standard.compileAndAnalyze(p, q)



  }


}
