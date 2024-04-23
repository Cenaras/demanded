import org.scalatest.funsuite.AnyFunSuite

class TestMagic extends AnyFunSuite {

  
  test("Souffle vs implementation") {
    val seed = 12
    val size = 7
    val vars = 3
    val fields = 1
    
    
    val g = ProgramGenerator(seed, vars, size, fields)
    val p = g.generate()
    p.print()
  }
  
  
}
