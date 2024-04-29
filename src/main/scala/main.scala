


@main
def main(): Unit = {
//    difference()
//    compareMagicToHeintzeTardieu(100, 7, 3, 1)
  datalogAnalysisCost()
}

private def datalogAnalysisCost(): Unit = {
  val times = 100
  val size = 7
  val vars = 3
  val fields = 1

  var standardCheaper = 0
  var alt1Cheaper = 0

  for i <- 0 until times do
    val seed = scala.util.Random.nextInt()
    val g = ProgramGenerator(seed, vars, size, fields)
    val p = g.generate()
    val q = g.genQuery

    val standard = Standard()
    standard.compileAndAnalyze(p, q)
    val standardCost = standard.cost

    val alt1 = Alt1()
    alt1.compileAndAnalyze(p, q)
    val alt1Cost = alt1.cost

    if standardCost < alt1Cost then
      standardCheaper+=1
    if alt1Cost < standardCost then
      alt1Cheaper+=1

  println(s"Standard was cheapest $standardCheaper times")
  println(s"Alt1 was cheapest $alt1Cheaper times")
}


private def difference(): Unit = {

  // Difference for query 1
  val q = 1
  val p = Parser.ParseTemplate("ht_magic_diff")
  val ex = NaiveExhaustiveSolver()
  val exSol = ex.solve(p)
  println("Exhaustive")
  ex.printSolution()

  val ht = HeintzeTardieu()
  val htSol = ht.solve(p, q)

  println("HeintzeTardieu")
  ht.printSolution()


  val fullfs = FullFS()
  val fullfsSol = fullfs.solve(p, q)

  println("FullFS")
  fullfs.printSolution()

  val magic = MagicSets()
  val mSol = magic.solve(p, q)

  println("MagicSets")
  magic.printSolution()

  println("Are MagicSets and HeintzeTardieu solutions identical for all variables?")
  println(s"\t${equalSolutions(htSol, mSol)}")


  val datalogAnalysis = Standard()
  datalogAnalysis.compileAndAnalyze(p, 1)
  datalogAnalysis.outputSolution("untitled/diff_mebe")
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
