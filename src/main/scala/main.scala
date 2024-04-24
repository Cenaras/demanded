


@main
def main(): Unit = {

  // Difference for query 1
  val p = Parser.ParseTemplate("ht_magic_diff_mebe")
  val ex = NaiveExhaustiveSolver()
  val exSol = ex.solve(p)
  println("Exhaustive")
  ex.printSolution()

  val ht = HeintzeTardieu()
  val htSol = ht.solve(p, 1)

  println("HeintzeTardieu")
  ht.printSolution()

  val magic = MagicSets()
  val mSol = magic.solve(p, 1)

  println("MagicSets")
  magic.printSolution()

  println("Are MagicSets and HeintzeTardieu solutions identical for all variables?")
  println(s"\t${equalSolutions(htSol, mSol)}")

//  compareMagicToHeintzeTardieu(100, 7, 3, 1)

  DatalogCompiler.compileAndAnalyze(p, 1)
  DatalogCompiler.solutionToSingleTSV("untitled/diff_mebe")


}


private def compareMagicToHeintzeTardieu(times: Int, size: Int, vars: Int, fields: Int): Unit = {
  for i <- 0 until times do
    val seed = scala.util.Random.nextInt()
    val g = ProgramGenerator(seed, vars, size, fields)
    val p = g.generate()
    val q = g.genQuery

    val ht = HeintzeTardieu()
    val magic = MagicSets()

    val htSol = ht.solve(p, q)
    val magicSol = magic.solve(p, q)

    if !equalSolutions(htSol, magicSol) then
      println(s"Different solutions with query $q!")
      p.print()
      println("Solution from HeintzeTardieu")
      ht.printSolution()
      println("Solution from MagicSets")
      magic.printSolution()
      throw Error()

}
def equalSolutions(sol1: Solution, sol2: Solution): Boolean = {
  for (c, s) <- sol1 do
    val otherSol = sol2(c)
    if s.size != otherSol.size then return false
    for t <- otherSol do
      if !s.contains(t) then return false
  true
}
