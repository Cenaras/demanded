import main.constraint.{ConstraintVar, ConstraintVariables}

object Util {
  def getCvar(solution: ConstraintVariables, id: Int, base: Boolean): ConstraintVar = {
    solution.find(c => c.getId == id && base == c.base).get;
  }

  /**
   * Asserts that the solution to a constraint variable is exactly the sequence of tokens provided.
   * @param cvar constraint variable to test
   * @param expectedIds sequence of tokens expected in the solution of the constraint variable
   */
  def assertTokenIds(cvar: ConstraintVar, expectedIds: Seq[Int]): Boolean = {
    var result: Boolean = true;

    val tokenIds: Seq[Int] = cvar.solution.map(f => f.id).toSeq

    assert(expectedIds.size == tokenIds.size)

    for (t <- expectedIds) {
      result &= tokenIds.contains(t)
    }

    result
  }


  def assertSolutions(exhaustive: ConstraintVariables, demanded: ConstraintVariables, query: Int): Boolean = {

    val demandedCvar = getCvar(demanded, query, true);
    val exhaustiveCvar = getCvar(exhaustive, query, true)

    val exhaustiveTokenSequence = exhaustiveCvar.solution.map(t => t.id).toSeq

    assertTokenIds(demandedCvar, exhaustiveTokenSequence)

  }

}
