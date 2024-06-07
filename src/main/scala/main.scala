


@main
def main(): Unit = {
//    difference()
//    compareMagicToHeintzeTardieu(100, 7, 3, 1)
//  datalogAnalysisCost()

  val p = Parser.ParseTemplate("MagicAlt1Compare")
  val q = 2
  val alt1 = Alt1()
  alt1.compileAndAnalyze(p, q)
  alt1.outputSolution("untitled/alt1Sol.tsv")

  val standard = Standard()
  standard.compileAndAnalyze(p, q)
  standard.outputSolution("untitled/standardSol.tsv")


}

// TODO: Implement these in the framework and compare then in the CompareDemanded test case
private def datalogAnalysisCost(): Unit = {
  val times = 1000
  val size = 7
  val vars = 3
  val fields = 1

  var standardCheaper = 0
  var alt1Cheaper = 0
  var htCheaper = 0
  var alt2Cheaper = 0

  var standardSolSizeSmallest = 0
  var alt1SolSizeSmallest = 0
  var htSolSizeSmallest = 0
  var alt2SolSizeSmallest = 0

  for i <- 0 until times do
    val seed = scala.util.Random.nextInt()
    val g = SimpleProgramGenerator(seed, vars, size, fields)
    val p = g.generate()
    val q = g.genQuery

    val standard = Standard()
    val standardOutPath = "untitled/stdOut.tsv"
    standard.compileAndAnalyze(p, q)
    standard.outputSolution(standardOutPath)
    val standardCost = standard.cost(standardOutPath)

    val alt1 = Alt1()
    val alt1OutPath = "untitled/alt1Out.tsv"
    alt1.compileAndAnalyze(p, q)
    alt1.outputSolution(alt1OutPath)
    val alt1Cost = alt1.cost(alt1OutPath)

    val ht = HeintzeTardieu()
    val htSol = ht.solve(p, q)
    val htCost = ht.cost


    val alt2 = Alt2()
    val alt2OutPath = "untitled/alt2Out.tsv"
    alt2.compileAndAnalyze(p, q)
    alt2.outputSolution(alt2OutPath)
    val alt2Cost = alt2.cost(alt2OutPath)


    val (unique1, smallest1) = uniqueSmallest(List(standardCost, alt1Cost, htCost, alt2Cost))
    if unique1 then
      if smallest1 == standardCost then standardCheaper += 1
      if smallest1 == alt1Cost then alt1Cheaper += 1
      if smallest1 == htCost then htCheaper += 1
      if smallest1 == alt2Cost then alt2Cheaper += 1




    val htSolSize = htSol.foldLeft(0)((a, sol) => {a + sol.size})
    val standardSolSize = standard.readSolution(standardOutPath).linesIterator.size
    val alt1SolSize = alt1.readSolution(alt1OutPath).linesIterator.size
    val alt2SolSize = alt2.readSolution(alt2OutPath).linesIterator.size

     val (unique2, smallest2) = uniqueSmallest(List(htSolSize, standardSolSize, alt1SolSize, alt2SolSize))
    if unique2 then
      if smallest2 == htSolSize then htSolSizeSmallest += 1
      if smallest2 == standardSolSize then standardSolSizeSmallest += 1
      if smallest2 == alt1SolSize then alt1SolSizeSmallest += 1
      if smallest2 == alt2SolSize then alt2SolSizeSmallest += 1

  println(s"Standard was cheapest $standardCheaper times")
  println(s"Alt1 was cheapest $alt1Cheaper times")
  println(s"Heintze-Tardieu was cheapest $htCheaper times")
  println(s"Alt2 was cheapest $alt2Cheaper times")

  println(s"Standard solution was smallest $standardSolSizeSmallest times")
  println(s"Alt1 solution was smallest $alt1SolSizeSmallest times")
  println(s"Heintze-Tardieu solution was smallest $htSolSizeSmallest times")
  println(s"Alt2 solution was smallest $alt2SolSizeSmallest times")
}


def uniqueSmallest(list: List[Int]): (Boolean, Int) = {
  val smallest = list.min
  if list.count(_ == smallest) != 1 then
    return (false, -1)
  return (true, smallest)
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
    val g = SimpleProgramGenerator(seed, vars, size, fields)
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
