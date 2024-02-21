package main.solver

import main.constraint.{BaseConstraintVar, ConstraintVar, ConstraintVariables, FieldConstraintVar}

class BaseSolver {
  private val DEBUG = false

  protected def propagate(from: ConstraintVar, to: ConstraintVar): Boolean = {
    var changed = false
    changed |= to.addTokens(from.solution)
    if changed then debug("Processing propagation from %s to %s".format(from, to))
    changed
  }

  protected def debug(msg: String): Unit = {
    if (DEBUG) then println(msg)
  }
}

object SolverUtil {
  /**
   * Accumulates the amount of tokens present in each constraint variable in the solution
   */
  def solutionSize(solution: ConstraintVariables): Int = {
    var size = 0
    solution.foreach(c => {
      size += c.solution.size
    })
    size
  }

  /**
   * Returns the field constraint variable ⟦t.f⟧ from the solution for the given token id and field
   *
   * @param solution the solution to search in
   * @param id       id of the token t
   * @param field    field f
   * @return constraint variable ⟦t.f⟧
   */
  def getFieldCvar(solution: ConstraintVariables, id: Int, field: String): ConstraintVar = {
    solution.find {
      case FieldConstraintVar(token, fld) => token.id == id && field == fld
      case _ => false
    }.get
  }

  /**
   * Returns the base constraint variable ⟦x⟧ from the solution for the given variable id
   *
   * @param solution the solution to search in
   * @param id       the id of variable x
   * @return constraint variable ⟦x⟧
   */
  def getBaseCvar(solution: ConstraintVariables, id: Int): ConstraintVar = {
    solution.find {
      case BaseConstraintVar(varId) => varId == id
      case _ => false
    }.get
  }


  /**
   * Checks that the solution to a constraint variable is exactly the sequence of tokens provided.
   *
   * @param cvar        constraint variable to test
   * @param expectedIds sequence of tokens expected in the solution of the constraint variable
   */
  def containsExactly(cvar: ConstraintVar, expectedIds: Seq[Int]): Boolean = {
    var result: Boolean = true;
    val tokenIds: Seq[Int] = cvar.solution.map(f => f.id).toSeq
    if (!(expectedIds.size == tokenIds.size))
      return false

    for (t <- expectedIds) {
      result &= tokenIds.contains(t)
    }
    result
  }

  def containsExactly(solution: ConstraintVariables, id: Int, expectedIds: Seq[Int]): Boolean = {
    containsExactly(getBaseCvar(solution, id), expectedIds)
  }

  def containsExactly(solution: ConstraintVariables, tokenId: Int, field: String, expectedIds: Seq[Int]): Boolean = {
    containsExactly(getFieldCvar(solution, tokenId, field), expectedIds)
  }

  def isEmpty(cvar: ConstraintVar): Boolean = {
    cvar.solution.isEmpty
  }

  /**
   * Compares an exhaustive solution to a demanded solution and reports whether the demanded solution is the same as the 
   * exhaustive solution for the queried variable.
   *
   * @param exhaustive the exhaustive solution
   * @param demanded   the demanded solution
   * @param query      the queried constraint variable
   * @return true if the solutions are identical for the queried constraint variable, false otherwise.
   */
  def compareSolutions(exhaustive: ConstraintVariables, demanded: ConstraintVariables, query: QueryID): Boolean = {
    val (demandedCvar, exhaustiveCvar) = query match
      case x: Int => (getBaseCvar(demanded, x), getBaseCvar(exhaustive, x))
      case (t: Int, f: String) => (getFieldCvar(demanded, t, f), getFieldCvar(exhaustive, t, f))

    val exhaustiveTokenSequence = exhaustiveCvar.solution.map(t => t.id).toSeq
    containsExactly(demandedCvar, exhaustiveTokenSequence)
  }
}
