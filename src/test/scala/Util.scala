import main.constraint.{BaseConstraintVar, ConstraintVar, ConstraintVariables, FieldConstraintVar}

object Util {
  def getFieldCvar(solution: ConstraintVariables, id: Int, field: String): ConstraintVar = {
    solution.find {
      case FieldConstraintVar(token, fld) => token.id == id && field == fld
      case _ => false
    }.get
  }

  def getBaseCvar(solution: ConstraintVariables, id: Int): ConstraintVar = {
    solution.find {
      case BaseConstraintVar(varId) => varId == id
      case _ => false
    }.get
  }


  /**
   * Asserts that the solution to a constraint variable is exactly the sequence of tokens provided.
   *
   * @param cvar        constraint variable to test
   * @param expectedIds sequence of tokens expected in the solution of the constraint variable
   */
  def assertTokens(cvar: ConstraintVar, expectedIds: Seq[Int]): Boolean = {
    var result: Boolean = true;
    val tokenIds: Seq[Int] = cvar.solution.map(f => f.id).toSeq
    if (!(expectedIds.size == tokenIds.size))
      return false

    for (t <- expectedIds) {
      result &= tokenIds.contains(t)
    }
    result
  }

  def assertTokens(solution: ConstraintVariables, id: Int, expectedIds: Seq[Int]): Boolean = {
    assertTokens(getBaseCvar(solution, id), expectedIds)
  }

  def assertTokens(solution: ConstraintVariables, tokenId: Int, field: String, expectedIds: Seq[Int]): Boolean = {
    assertTokens(getFieldCvar(solution, tokenId, field), expectedIds)
  }


  def assertEmpty(cvar: ConstraintVar): Boolean = {
    cvar.solution.isEmpty
  }

  def assertSolutions(exhaustive: ConstraintVariables, demanded: ConstraintVariables, query: Int): Boolean = {

    val demandedCvar = getBaseCvar(demanded, query);
    val exhaustiveCvar = getBaseCvar(exhaustive, query)

    val exhaustiveTokenSequence = exhaustiveCvar.solution.map(t => t.id).toSeq

    assertTokens(demandedCvar, exhaustiveTokenSequence)
  }
}
