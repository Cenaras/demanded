import org.scalatest.funsuite.AnyFunSuite

class TestInputProgram extends AnyFunSuite {

  test("Alias") {
    val p = Parser.ParseTemplate("alias")
    val solver = NaiveExhaustiveSolver()
    val solution = solver.solve(p)

    TestUtil.assertSolution(1, List(1), solution)
    TestUtil.assertSolution((1, 1), List(2, 3), solution)
    TestUtil.assertSolution(2, List(2), solution)
    TestUtil.assertSolution(3, List(1), solution)
    TestUtil.assertSolution(4, List(3), solution)
    TestUtil.assertSolution(5, List(2, 3), solution)
  }


  test("Load/Store") {
    val p = Parser.ParseTemplate("load_store")
    val solver = NaiveExhaustiveSolver()
    val solution = solver.solve(p)

    TestUtil.assertSolution(1, List(1, 2), solution)
    TestUtil.assertSolution(2, List(2), solution)
    TestUtil.assertSolution((1,1), List(2), solution)

  }

  test("Multiple Fields") {
    val p = Parser.ParseTemplate("multiple_fields")
    val solver = NaiveExhaustiveSolver()
    val solution = solver.solve(p)

    TestUtil.assertSolution(1, List(1), solution)
    TestUtil.assertSolution((1, 1), List(2), solution)
    TestUtil.assertSolution((2,2), List(1), solution)
    TestUtil.assertSolution((3, 1), List(4), solution)
    TestUtil.assertSolution((3,2), List(4), solution)


  }

}
