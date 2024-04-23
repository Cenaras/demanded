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


}
