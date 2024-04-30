import org.scalatest.funsuite.AnyFunSuite

class CompareDemanded extends AnyFunSuite {
  // Ensuring that FullFS is not smarter than Heintze-Tardieu. This shows that MagicSets is better than both
  test("Heintze-Tardieu vs FullFS") {
    TestUtil.compareDemandedSolvers(100000, TestUtil.SolverType.HT, TestUtil.SolverType.Magic)
  }

  test("MagicSets vs Alt1 on solutions") {
    TestUtil.compareDemandedSolvers(100000, TestUtil.SolverType.Magic, TestUtil.SolverType.Alt1)
  }

  // Concrete test case showing that Heintze-Tardieu and MagicSets do no compute the same results
  test("Both non-optimal compared to MagicSets") {
    val p = Parser.ParseTemplate("ht_magic_diff")
    val q = 1
    val htSol = HeintzeTardieu().solve(p, q)
    val fsSol = FullFS().solve(p, q)

    assert(htSol == fsSol)

    val magicSol = MagicSets().solve(p, q)
    assert(!(htSol == magicSol))
    assert(!(fsSol == magicSol))

    assert(magicSol.size < Math.min(htSol.size, fsSol.size))
  }

  test("Compare solution sizes") {
    var htSmallest = 0
    var magicSmallest = 0
    var htCheapest = 0
    var magicCheapest = 0

    for i <- 0 until 100000 do
      if i != 0 && i % 10000 == 0 then println(s"Completed $i solution comparison tests")
      val g = ProgramGenerator(scala.util.Random.nextInt(), 5, 15, 2)
      val p = g.generate()
      val q = g.genQuery

      val ht = HeintzeTardieu()
      val magic = MagicSets()

      val htSol = ht.solve(p, q)
      val magicSol = magic.solve(p, q)

      val htSolSize = htSol.foldLeft(0)((a, b) => a+b.size)
      val magicSolSize = magicSol.foldLeft(0)((a, b) => a + b.size)

      val htCost = ht.cost
      val magicCost = magic.cost

      if htSolSize < magicSolSize then htSmallest += 1
      if magicSolSize < htSolSize then magicSmallest += 1

      if htCost < magicCost then htCheapest += 1
      if magicCost < htCost then magicCheapest += 1

    println("\nSolution comparison results:")
    println(s"\tHeintze-Tardieu smallest: $htSmallest times")
    println(s"\tMagic sets smallest: $magicSmallest times")
    println("\nCost comparison results:")
    println(s"\tHeintze-Tardieu cheapest: $htCheapest times")
    println(s"\tMagic sets cheapest: $magicCheapest times")

  }


}
