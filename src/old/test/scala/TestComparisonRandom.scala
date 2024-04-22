//import TestUtil.{newDist, newGenerator, repeatTest}
//import org.scalatest.funsuite.AnyFunSuite
//
//
//class TestComparisonRandom extends AnyFunSuite {
//
//
//  test("Small programs") {
//    repeatTest(20000, newGenerator(5, 2, 15, newDist(20, 50, 20, 10)))
//  }
//
//  test("Small function calls") {
//    repeatTest(20000, newGenerator(5, 2, 10, newDist(10, 30, 10, 10, 20, 20)))
//  }
//
//  test("Medium Load/Store heavy") {
//    repeatTest(20000, newGenerator(7, 3, 10, newDist(20, 20, 30, 30)))
//  }
//
//  test("Medium programs") {
//    repeatTest(20000, newGenerator(10, 3, 25, newDist(20, 50, 20, 10)))
//  }
//
//  test("Medium function call") {
//    repeatTest(20000, newGenerator(10, 3, 25, newDist(10, 30, 10, 10, 20, 20)))
//  }
//
//  test("Random program") {
//    repeatTest(2000, newGenerator(100, 30, 200, newDist(20, 50, 20, 10)))
//  }
//
//  test("Load/Store heavy") {
//    repeatTest(300, newGenerator(250, 50, 500, newDist(25, 25, 25, 25)))
//  }
//
//  // FIXME: Test takes a very long time to terminate even for only 1 repeated test - why is this the case?
//  //    test("Large") {
//  //      repeatTest(1, newGenerator(100, 20, 2500, newDist(25, 35, 20, 20)))
//  //    }
//
//  // FIXME: Same issue for this - do some diagnostics to figure out if it is only solving. In that case, not much can
//  //  be done with the naive solver algorithm...
//  test("Function call programs") {
//    repeatTest(50, newGenerator(10, 5, 250, newDist(5, 10, 5, 5, 35, 40)))
//  }
//}
