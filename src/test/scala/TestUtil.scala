import TestUtil.GeneratorType.Simple
import TestUtil.SolverType.{Alias, Alt1, HTImp}
import com.sun.org.apache.bcel.internal.generic.AALOAD

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
  
  def compareSolutionsForQuery(ex: Solution, de: Solution, query: Cell): Boolean = {
    val querySol = de(query)
    containsExactly(query, querySol.toList, ex)
  }

  def assertSolution(x: Cell, tokens: List[Token], sol: Solution): Unit = {
    assert(containsExactly(x, tokens, sol))
  }

  enum SolverType:
    case HT, Magic, FullFS, Alt1, HTImp, Alias

  enum GeneratorType:
    case Simple, Initialized
  

  def demandedSolver(st: SolverType): DemandedSolver = {
    st match
      case SolverType.HT => HeintzeTardieu()
      case SolverType.Magic => MagicSets()
      case SolverType.FullFS => FullFS()
      case SolverType.Alt1 => MagicAlt1()
      case SolverType.HTImp => ImprovedHeintzeTardieu()
      case SolverType.Alias => AliasBased()
  }

  def randomTest(size: Int, vars: Int, fields: Int): (Program, Cell) = {
    val seed = scala.util.Random.nextInt()
    val g = new SimpleProgramGenerator(seed, vars, size, fields)
    val p = g.generate()
    val query = g.genQuery
    (p, query)
  }

  // Compare demanded solvers for small programs
  def compareDemandedSolvers(times: Int, sol1Type: SolverType, sol2Type: SolverType): Unit = {
    compareDemandedSolvers(times, 7, 3, 1, sol1Type, sol2Type)
  }

  def compareDemandedSolvers(times: Int, size: Int, vars: Int, fields: Int, sol1Type: SolverType, sol2Type: SolverType): Unit = {
    for _ <- 0 until times do
      val (p, q) = randomTest(size, vars, fields)

      val solver1 = demandedSolver(sol1Type)
      val solver2 = demandedSolver(sol2Type)

      val sol1 = solver1.solve(p, q)
      val sol2 = solver2.solve(p, q)

      if !compareSolutionsForQuery(sol1, sol2, q) then
        p.print()
        println("Query: " + q)
        println(s"$sol1Type solution: ")
        println(sol1)
        println(s"$sol2Type solution: ")
        println(sol2)

        solver2 match
          case a: ImprovedHeintzeTardieu => println("write reachable: \n" + a.write_reachable)
          case _ =>
        throw new Error("mismatch")
  }

}
