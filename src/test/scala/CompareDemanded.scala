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
    var alt1Smallest = 0

    var htCheapest = 0
    var magicCheapest = 0
    var alt1Cheapest = 0


    for i <- 1 to 1000000 do
      if i % 25000 == 0 then println(s"Completed $i solution comparison tests")
      val g = ProgramGenerator(scala.util.Random.nextInt(), 5, 15, 2)
      val p = g.generate()
      val q = g.genQuery

      val ht = HeintzeTardieu()
      val magic = MagicSets()
      val alt1 = MagicAlt1()

      val htSol = ht.solve(p, q)
      val magicSol = magic.solve(p, q)
      val alt1Sol = alt1.solve(p, q)

      val htSolSize = htSol.foldLeft(0)((a, b) => a+b.size)
      val magicSolSize = magicSol.foldLeft(0)((a, b) => a + b.size)
      val alt1SolSize = alt1Sol.foldLeft(0)((a,b) => a+b.size)

      TestUtil.compareSolutionsForQuery(htSol, magicSol, q)
      TestUtil.compareSolutionsForQuery(magicSol, htSol, q)
      TestUtil.compareSolutionsForQuery(magicSol, alt1Sol, q)

      val (uniqueSize, smallestSize) = uniqueSmallest(List(htSolSize, magicSolSize, alt1SolSize))
      if uniqueSize then
        if smallestSize == htSolSize then htSmallest += 1
        if smallestSize == magicSolSize then magicSmallest += 1
        if smallestSize == alt1SolSize then alt1Smallest += 1

      val htCost = ht.cost
      val magicCost = magic.cost
      val alt1Cost = alt1.cost

      val (uniqueCost, smallestCost) = uniqueSmallest(List(htCost, magicCost, alt1Cost))
      if uniqueCost then
        if smallestCost == htCost then htCheapest += 1
        if smallestCost == magicCost then magicCheapest += 1
        if smallestCost == alt1Cost then alt1Cheapest += 1


    println("\nSolution comparison results:")
    println(s"\tHeintze-Tardieu smallest: $htSmallest times")
    println(s"\tMagic sets smallest: $magicSmallest times")
    println(s"\tAlt1 smallest: $alt1Smallest times")
    println("\nCost comparison results:")
    println(s"\tHeintze-Tardieu cheapest: $htCheapest times")
    println(s"\tMagic sets cheapest: $magicCheapest times")
    println(s"\tAlt1 cheapest: $alt1Cheapest times")

  }

  private def uniqueSmallest(values: List[Int]): (Boolean, Int) = {
    val smallest = values.min
    if values.count(_ == smallest) == 1 then
      return (true, smallest)
    else
      return (false, -1)
  }


}
