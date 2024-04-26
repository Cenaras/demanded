import javax.management.Query
import scala.collection.mutable

object TestUtil {

  def containsExactly(x: Cell, tokens: List[Token], sol: Solution): Boolean = {
    val res = sol(x)
    var same = true
    same = same & res.size == tokens.size
    for t <- res do
      same = same & tokens.contains(t)
    same
  }
  
  def compareSolutions(ex: Solution, de: Solution, query: Cell): Boolean = {
    val querySol = de(query)
    containsExactly(query, querySol.toList, ex)
  }

  def assertSolution(x: Cell, tokens: List[Token], sol: Solution): Unit = {
    assert(containsExactly(x, tokens, sol))
  }
  
  enum SolverType:
    case HT, Magic, FullFS


  def demandedSolver(st: SolverType): DemandedSolver = {
    st match
      case SolverType.HT => HeintzeTardieu()
      case SolverType.Magic => MagicSets()
      case SolverType.FullFS => FullFS()
  }

  def randomTest(size: Int, vars: Int, fields: Int): (Program, Cell) = {
    val seed = scala.util.Random.nextInt()
    val g = new ProgramGenerator(seed, vars, size, fields)
    val p = g.generate()
    val query = g.genQuery
    (p, query)
  }

  // Compare demanded solvers for small programs
  def compareDemandedSolvers(times: Int, sol1Type: SolverType, sol2Type: SolverType): Unit = {
    for _ <- 0 until times do
      val (p, q) = randomTest(7, 3, 1)
      val sol1 = demandedSolver(sol1Type).solve(p, q)
      val sol2 = demandedSolver(sol2Type).solve(p, q)

      if !compareSolutions(sol1, sol2, q) then
        throw new Error("mismatch")
  }

}
