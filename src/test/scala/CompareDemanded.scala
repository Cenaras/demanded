import org.scalatest.funsuite.AnyFunSuite

class CompareDemanded extends AnyFunSuite {
  // Ensuring that FullFS is not smarter than Heintze-Tardieu. This shows that MagicSets is better than both
  test("Heintze-Tardieu vs FullFS") {
    TestUtil.compareDemandedSolvers(100000, TestUtil.SolverType.HT, TestUtil.SolverType.Magic)
  }

  // Concrete test case showing that Heintze-Tardieu and MagicSets do no compute the same results
  test("Both non-optimal compared to MagicSets") {
    val p = Parser.ParseTemplate("ht_magic_diff_mebe")
    val q = 1
    val htSol = HeintzeTardieu().solve(p, q)
    val fsSol = FullFS().solve(p, q)

    assert(htSol == fsSol)

    val magicSol = MagicSets().solve(p, q)
    assert(!(htSol == magicSol))
    assert(!(fsSol == magicSol))

    assert(magicSol.size < Math.min(htSol.size, fsSol.size))


  }


}
