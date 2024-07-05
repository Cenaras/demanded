import org.scalatest.funsuite.AnyFunSuite

class MagicVariants extends AnyFunSuite {

  // Results for 1.000.000 runs with distribution (5, 15, 2)
  // Value v at index i means that analysis i cost the most for v programs

  /** Most expensive computation - removing the worst performing in each iteration */

  // m0, m1, m2, m6, m7
  //  most expensive:
  //  (1139, 5181, 591433, 18743, 24599)
  //  cheapest:
  //  (21328, 13378, 330, 35170, 29368)


  // m0, m1, m6, m7
  //  most expensive:
  //  (72435, 44938, 21400, 28308)
  //  cheapest:
  //  (21944, 13518, 35149, 29515)

  // From here the two runs do not follow so we split them up
  // Expensive results:
  // m1, m6, m7
  //  most expensive:
  //  (467330, 29217, 29909)

  // m6, m7
  //  most expensive:
  //  (51849, 63341)

  // Cheapest results:
  // m0, m6, m7
  // cheapest:
  //  (181318, 36107, 32689)

  // m0, m6
  // cheapest:
  //  (185694, 487007)



  test("Compare costs") {
    // Amount of times index i was the most expensive
    val highest: Array[Int] = new Array[Int](5)
    // Amount of times index i was the cheapest
    val lowest: Array[Int] = new Array[Int](5)

    for i <- 0 until 1000000 do {

      if i != 0 && i % 10000 == 0 then
        println(s"Finished ${i} iterations")

      val seed = scala.util.Random.nextInt() // 5, 15, 2
      val g = new SimpleProgramGenerator(seed, 5, 15, 2)
      val p = g.generate()
      val q = g.genQuery

      val m0 = new MagicSet0()
      val sol = m0.solve(p, q)
      val m1 = new MagicSet1()
      m1.solve(p, q)
      val m2 = new MagicSet2()
      m2.solve(p, q)
      val m6 = new MagicSet6()
      m6.solve(p, q)
      val m7 = new MagicSet7()
      m7.solve(p, q)

      val costs = Array(m0.cost, m1.cost, m2.cost, m6.cost, m7.cost)
      var largest = -1
      var maxIndex = -1
      for (i <- costs.indices) {
        if (costs(i) > largest) {
          largest = costs(i)
          maxIndex = i
        }
      }

      // Check if largest value was unique
      val maxOccurrences = costs.count(p => p == largest)
      if (maxOccurrences == 1) {
        highest(maxIndex) += 1
      }

      var smallest = Int.MaxValue
      var minIndex = -1
      for (i <- costs.indices) {
        if (costs(i) < smallest) {
          smallest = costs(i)
          minIndex = i
        }
      }

      val occurrences = costs.count(p => p == smallest)
      if (occurrences == 1) {
        lowest(minIndex) += 1
      }

    }

    println("Distribution of most expensive where solution must have been uniquely the largest")
    println(highest.mkString("Array(", ", ", ")"))
    println()
    println("Distribution of cheapest where solution must have been uniquely the smallest")
    println(lowest.mkString("Array(", ", ", ")"))

  }

  test("temp") {
    val seed = -1008993739
    val g = new SimpleProgramGenerator(seed, 3, 5, 1)
    val p = g.generate()
    val q = 2
    p.print()

    val ex = new NaiveExhaustiveSolver()
    val m = new MagicSet2()

    val exSol = ex.solve(p)
    val mSol = m.solve(p, q)

    println(exSol)
    println(mSol)

  }


}
