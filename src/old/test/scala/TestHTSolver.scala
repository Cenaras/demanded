//import TestUtil.solveDemanded
//import main.program.*
//import main.solver.HTSolver
//import main.solver.SolverUtil.{containsExactly, getBaseCvar, getFieldCvar, isEmpty}
//import org.scalatest.funsuite.AnyFunSuite
//
//class TestHTSolver extends AnyFunSuite {
//
//  test("LoadStore") {
//    val solution = solveDemanded(ProgramTemplates.LoadStore, 1, HTSolver())
//
//    val x1 = getBaseCvar(solution, 1)
//    containsExactly(x1, Seq(1, 2))
//  }
//
//  test("Aliasing") {
//    val solution = solveDemanded(ProgramTemplates.Aliasing, 5, HTSolver())
//
//    containsExactly(getBaseCvar(solution, 5), Seq(2, 3))
//  }
//
//  test("Demanded") {
//    val solution = solveDemanded(ProgramTemplates.DemandedSimple, 1, HTSolver())
//
//    // x1 holds t1 and t2 by demanded. x3 should be empty as it is not required to compute x1
//    containsExactly(getBaseCvar(solution, 1), Seq(1, 2))
//    assert(getBaseCvar(solution, 3).solution.isEmpty)
//
//  }
//
//  test("Transitive Token Tracking") {
//    val solution = solveDemanded(ProgramTemplates.TransitiveTokenTracking, 3, HTSolver())
//
//    assert(containsExactly(getBaseCvar(solution, 3), Seq(0, 1)))
//  }
//
//  test("Multiple Fields") {
//    val solution = solveDemanded(ProgramTemplates.MultipleFields, (1, "f"), HTSolver())
//
//    assert(containsExactly(solution, 1, Seq(1)))
//    assert(containsExactly(solution, 1, "f", Seq(2)))
//    assert(containsExactly(solution, 2, "g", Seq(1)))
//    assert(isEmpty(getFieldCvar(solution, 3, "f")))
//    assert(isEmpty(getFieldCvar(solution, 3, "g")))
//  }
//
//  test("FunCall") {
//    val solution = solveDemanded(ProgramTemplates.FunCall, 8, HTSolver())
//    assert(containsExactly(solution, 8, Seq(3)))
//  }
//}
