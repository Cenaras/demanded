import scala.collection.mutable

object TestUtil {

  def assertSolution(x: Cell, tokens: List[Token], sol: Solution) = {
    val res = sol(x)
    assert(res.size == tokens.size)
    for t <- res do 
      assert(tokens.contains(t))
  }
  
}
