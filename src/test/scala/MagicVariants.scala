import org.scalatest.funsuite.AnyFunSuite

class MagicVariants extends AnyFunSuite {


  test("qwe") {

    for _ <- 0 until 100000 do {
      val seed = scala.util.Random.nextInt() // 5, 15, 2
      val g = new SimpleProgramGenerator(seed, 5, 15, 2)
      val p = g.generate()
      val q = g.genQuery

      val ex = new NaiveExhaustiveSolver()
      val m2 = new MagicSet2()

      val exSol = ex.solve(p)
      val m2Sol = m2.solve(p, q)

      if !(exSol.get(q) == m2Sol.get(q)) then
        println(s"Mismatch in solutions for program with query ${q} and seed ${seed}:")
        p.print()
        println("ExSol:")
        println(exSol)
        println("m1Sol:")
        println(m2Sol)
        throw Error()
    }

  }

  test("temp") {
    val seed = -1008993739
    val g = new SimpleProgramGenerator(seed, 3, 5, 1)
    val p = g.generate()
    val q = 2
    p.print()

    val ex = new NaiveExhaustiveSolver()
    val m = new MagicSet2()

    val exSol = ex.solve(p)
    val mSol = m.solve(p, q)

    println(exSol)
    println(mSol)

  }


}
