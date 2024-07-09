import org.scalatest.funsuite.AnyFunSuite

class MagicVariants extends AnyFunSuite {

  // Results for 1.000.000 runs with distribution (5, 15, 2)
  // Value v at index i means that analysis i cost the most for v programs

  /** Most expensive computation - removing the worst performing in each iteration */
  /** Cheapest computation - removing worst performing in each iteration */
  /** The two metrics start to deviate after some iterations, so therefore the results are separated when this happens */

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


  /** Results where ties are counted in all entries rather than only when uniquely largest/smallest */
  // m0, m1, m2, m6, m7
  //  most expensive:
  //    (273102, 277533, 868140, 369430, 375658)
  //  cheapest:
  //    (502002, 498111, 360456, 777980, 772653)


  // m0, m1, m6, m7
  //  most expensive:
  //    (768088, 741220, 479484, 488202)
  //  cheapest:
  //    (502047, 498421, 777879, 771798)


  /* Results for most expensive on the remaining strategies */

  //  m1, m6, m7
  //  most expensive:
  //    (800418, 498656, 498284)

  // m6, m7
  //  most expensive:
  //    (936498, 948161)

  // Winner: m6


  /* Results for cheapest on the remaining strategies */
  //  m0, m6, m7
  //  cheapest:
  //    (508855, 783722, 778787)

  // m6, m7
  //  cheapest:
  //    (948407, 936015)

  // Winner: m6


  /** Results for larger experiments: Distribution (15, 300, 4) for 100 iterations - counting all ties.
   * Using program distribution from Wave Propagation Paper - (14, 49, 25, 12) */

  // m0, m1, m2, m6, m7
  //  expensive:
  //    (0, 0, 100, 0 , 0)
  //  cheap:
  //    (0, 0, 0, 100, 100)


  // m0, m1, m6, m7
  //  expensive:
  //    (100, 100, 0, 0)
  // cheap:
  //    (0, 0, 100, 100)


  // m6, m7
  //  expensive:
  //    (100, 100)
  //  cheap:
  //    (100, 100)


  // It seems like m6 and m7 perform the same work when programs get large enough
  // and it seems like they are performing better than the other formulations always for large programs


  test("Compare costs") {
    // Amount of times index i was the most expensive
    val highest: Array[Int] = new Array[Int](5)
    // Amount of times index i was the cheapest
    val lowest: Array[Int] = new Array[Int](5)

    for i <- 0 until 1000000 do {


      val seed = scala.util.Random.nextInt() // 5, 15, 2
      val g = new SimpleProgramGenerator(seed, 5, 15, 2)
      val p = g.generate()
      val q = g.genQuery

      val m0 = new MagicSet0()
      m0.solve(p, q)
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

      //       Check if largest value was unique
      //      val maxOccurrences = costs.count(p => p == largest)
      //      if (maxOccurrences == 1) {
      //        highest(maxIndex) += 1
      //      }


      // For ties, count both
      for (i <- costs.indices) {
        if (costs(i) == largest) {
          highest(i) += 1
        }
      }


      var smallest = Int.MaxValue
      var minIndex = -1
      for (i <- costs.indices) {
        if (costs(i) < smallest) {
          smallest = costs(i)
        }
      }


      //      val occurrences = costs.count(p => p == smallest)
      //      if (occurrences == 1) {
      //        lowest(minIndex) += 1
      //      }

      //For ties, count both
      for (i <- costs.indices) {
        if (costs(i) == smallest) {
          lowest(i) += 1
        }
      }

    }

    println("Distribution of most expensive where solution must have been uniquely the largest")
    println(highest.mkString("Array(", ", ", ")"))
    println()
    println("Distribution of cheapest where solution must have been uniquely the smallest")
    println(lowest.mkString("Array(", ", ", ")"))

  }

  test("temp") {
    val seed = scala.util.Random.nextInt()
    val g = new SimpleProgramGenerator(seed, 15, 250, 5)
    val p = g.generate()
    val q = 2
    //    p.print()

    val ex = new NaiveExhaustiveSolver()
    val m = new MagicSet6()

    val exSol = ex.solve(p)
    val mSol = m.solve(p, q)

    //    println(exSol)
    //    println(mSol)

    println(exSol == mSol)

  }


}
