package main.solver

import main.constraint.*

import scala.collection.mutable

trait Exhaustive {

}


/** A query is either for a base constraint variable (denoted by varId)
 * or a field constraint variable (denoted by (tokenId, field)) */
type QueryID = Int | (Int, String)

trait Demanded extends BaseSolver {
  /** List of queried constraint variables. */
  val Q: mutable.Set[ConstraintVar] = mutable.Set()
  /** List of tracked tokens. Tracked tokens must be propagated regardless of demand. */
  val W: mutable.Set[Token] = mutable.Set()

  /**
   * Adds a constraint variable to demand, and optionally uses the constraint causing this for debugging
   *
   * @param constraintVar the constraint variable to add to demand
   * @param constraint    Optional constraint that caused the demand
   * @return true if the element was added, false if it was already present.
   */
  def addDemand(constraintVar: ConstraintVar, constraint: Option[Constraint]): Boolean = {
    if (Q.add(constraintVar)) {
      debug("Adding %s to demand due to %s".format(constraintVar, constraint))
      return true
    }
    false
  }

  /**
   * Adds a token to the set of tracked tokens and optionally uses the constraint causing this for debugging
   *
   * @param token      the token to track
   * @param constraint Optional constraint that caused the tracking
   * @return true if the token was added, false if it was already present
   */
  def addTracking(token: Token, constraint: Option[Constraint]): Boolean = {
    if (W.add(token)) {
      debug("Adding %s to tracking due to %s".format(token, constraint))
      return true
    }
    false
  }


  /**
   * Adds the queried constraint variable to demand. Call this as the first method in the solve method.
   *
   * @param queryID     the id of the query
   * @param constraints the constraints
   */
  def demandQuery(queryID: QueryID, constraints: ConstraintEnvironment): Unit = {
    val queriedCvar = queryID match
      case (t, f) =>
        W.add(constraints.id2Token(t))
        constraints.tf2Cvar.get(constraints.id2Token(t), f)

      case x: Int => constraints.id2Cvar.get(x)

    queriedCvar match
      case None => throw Error("Queried constraint variable %s does not exist".format(queryID))
      case Some(v) => addDemand(v, None)

  }

  def solve(constraints: ConstraintEnvironment, queryID: QueryID): ConstraintVariables
}
