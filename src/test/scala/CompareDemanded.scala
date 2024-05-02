import TestUtil.SolverType.HTImp
import org.scalatest.funsuite.AnyFunSuite

class CompareDemanded extends AnyFunSuite {
  // Ensuring that FullFS is not smarter than Heintze-Tardieu. This shows that MagicSets is better than both
  test("Heintze-Tardieu vs Magic") {
    TestUtil.compareDemandedSolvers(100000, TestUtil.SolverType.HT, TestUtil.SolverType.Magic)
  }

  test("MagicSets vs Alt1 on solutions") {
    TestUtil.compareDemandedSolvers(100000, TestUtil.SolverType.Magic, TestUtil.SolverType.Alt1)
  }

  test("Heintze-Tardieu Variants") {
    TestUtil.compareDemandedSolvers(100000, 12, 5, 2, TestUtil.SolverType.HT, TestUtil.SolverType.HTImp)
    TestUtil.compareDemandedSolvers(100000, 12, 5, 2, TestUtil.SolverType.HT, TestUtil.SolverType.FullFS)
    TestUtil.compareDemandedSolvers(100000, 12, 5, 2, TestUtil.SolverType.FullFS, TestUtil.SolverType.HTImp)
  }

  /** A (now deleted) test shows that Improved Heintze-Tardieu is always better in terms of solution sizes,
   * but naturally does pay the extra cost of storing all write-reachable cells, which is typically dataflow facts than
   * the improved solution size saves. This is to compare how this is reflected against the Magic Sets implementation*/
  test("Improved Heintze-Tardieu vs Magic Sets") {
    var impSolSmallest = 0
    var magicSolSmallest = 0

    var impCheapest = 0
    var magicCheapest = 0

    for _ <- 0 until 100000 do
      val imp = ImprovedHeintzeTardieu()
      val magic = MagicSets()
      val (p, q) = TestUtil.randomTest(15, 5, 3)

      val impSol = imp.solve(p, q)
      val magicSol = magic.solve(p, q)
      TestUtil.compareSolutionsForQuery(impSol, magicSol, q)

      val impSolSize = impSol.foldLeft(0)((a, b) => a + b.size)
      val magicSolSize = magicSol.foldLeft(0)((a, b) => a + b.size)

      if impSolSize < magicSolSize then impSolSmallest += 1
      if magicSolSize < impSolSize then magicSolSmallest += 1

      if imp.cost < magic.cost then impCheapest += 1
      if magic.cost < imp.cost then magicCheapest += 1


    println(s"Improved Heintze-Tardieu solution smallest: ${impSolSmallest}")
    println(s"Magic Sets solution smallest: ${magicSolSmallest}")

    println(s"Improved Heintze-Tardieu cheapest: ${impCheapest}")
    println(s"Magic Sets cheapest: ${magicCheapest}")

  }

  test("Solution size and cost") {
    var htSolSmallest = 0
    var htImpSolSmallest = 0

    var htCheapest = 0
    var htImpCheapest = 0

    for _ <- 0 until 100000 do
      val (p, q) = TestUtil.randomTest(10, 4, 2)
      val ht = HeintzeTardieu()
      val htImp = ImprovedHeintzeTardieu()

      val htSol = ht.solve(p, q)
      val htImpSol = htImp.solve(p, q)

      val htSolSize = htSol.foldLeft(0)((a, b) => a + b.size)
      val htImpSolSize = htImpSol.foldLeft(0)((a, b) => a + b.size)

      val htCost = ht.cost
      val htImpCost = htImp.cost

      if htCost < htImpCost then htCheapest += 1
      if htImpCost < htCost then htImpCheapest += 1

      if htSolSize < htImpSolSize then htSolSmallest += 1
      if htImpSolSize < htSolSize then htImpSolSmallest += 1


    println(s"Heintze-Tardieu solution smallest: ${htSolSmallest}")
    println(s"Improved Heintze-Tardieu solution smallest: ${htImpSolSmallest}")

    println(s"Heintze-Tardieu cheapest: ${htCheapest}")
    println(s"Improved Heintze-Tardieu cheapest: ${htImpCheapest}")

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
    var impHtSmallest = 0

    var htCheapest = 0
    var magicCheapest = 0
    var alt1Cheapest = 0
    var impHtCheapest = 0


    for i <- 1 to 100000 do
      if i % 25000 == 0 then println(s"Completed $i solution comparison tests")
      val g = ProgramGenerator(scala.util.Random.nextInt(), 5, 15, 2)
      val p = g.generate()
      val q = g.genQuery

      val ht = HeintzeTardieu()
      val magic = MagicSets()
      val alt1 = MagicAlt1()
      val impHt = ImprovedHeintzeTardieu()

      val htSol = ht.solve(p, q)
      val magicSol = magic.solve(p, q)
      val alt1Sol = alt1.solve(p, q)
      val impHtSol = impHt.solve(p, q)


      val htSolSize = htSol.foldLeft(0)((a, b) => a+b.size)
      val magicSolSize = magicSol.foldLeft(0)((a, b) => a + b.size)
      val alt1SolSize = alt1Sol.foldLeft(0)((a,b) => a+b.size)
      val impHtSolSize = impHtSol.foldLeft(0)((a,b) => a+b.size)

      TestUtil.compareSolutionsForQuery(htSol, magicSol, q)
      TestUtil.compareSolutionsForQuery(magicSol, htSol, q)
      TestUtil.compareSolutionsForQuery(magicSol, alt1Sol, q)
      TestUtil.compareSolutionsForQuery(htSol, impHtSol, q)

      val (uniqueSize, smallestSize) = uniqueSmallest(List(htSolSize, magicSolSize, alt1SolSize, impHtSolSize))
      if uniqueSize then
        if smallestSize == htSolSize then htSmallest += 1
        if smallestSize == magicSolSize then magicSmallest += 1
        if smallestSize == alt1SolSize then alt1Smallest += 1
        if smallestSize == impHtSolSize then impHtSmallest += 1

      val htCost = ht.cost
      val magicCost = magic.cost
      val alt1Cost = alt1.cost
      val impHtCost = impHt.cost

      val (uniqueCost, smallestCost) = uniqueSmallest(List(htCost, magicCost, alt1Cost, impHtCost))
      if uniqueCost then
        if smallestCost == htCost then htCheapest += 1
        if smallestCost == magicCost then magicCheapest += 1
        if smallestCost == alt1Cost then alt1Cheapest += 1
        if smallestCost == impHtCost then impHtCheapest += 1


    println("\nSolution comparison results:")
    println(s"\tHeintze-Tardieu smallest: $htSmallest times")
    println(s"\tMagic sets smallest: $magicSmallest times")
    println(s"\tAlt1 smallest: $alt1Smallest times")
    println(s"\tImproved Heintze-Tardieu smallest: $impHtSmallest times")
    println("\nCost comparison results:")
    println(s"\tHeintze-Tardieu cheapest: $htCheapest times")
    println(s"\tMagic sets cheapest: $magicCheapest times")
    println(s"\tAlt1 cheapest: $alt1Cheapest times")
    println(s"\tImproved Heintze-Tardieu cheapest: $impHtCheapest times")

  }

  private def uniqueSmallest(values: List[Int]): (Boolean, Int) = {
    val smallest = values.min
    if values.count(_ == smallest) == 1 then
      return (true, smallest)
    else
      return (false, -1)
  }


}
