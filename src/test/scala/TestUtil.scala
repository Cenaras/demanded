import scala.collection.mutable

object TestUtil {

  def assertSolution(x: Cell, tokens: List[Token], sol: Solution): Boolean = {
    val res = sol(x)
    var same = true
    same = same | res.size == tokens.size
    for t <- res do
      same = same | tokens.contains(t)
    same
  }


  def compareSolutions(ex: Solution, de: Solution, query: Cell): Boolean = {
    val querySol = de(query)
    assertSolution(query, querySol.toList, ex)
  }


}
