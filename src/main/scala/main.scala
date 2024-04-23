


@main
def main(): Unit = {

  val seed = 1
  val vars = 3
  val fields = 1
  val insn = 10

  val g = new ProgramGenerator(seed, vars, insn, fields)
  val p = g.generate()
  p.print()

  val exhaustiveSolver = NaiveExhaustiveSolver()
  val solution = exhaustiveSolver.solve(p)
  exhaustiveSolver.printSolution()




}