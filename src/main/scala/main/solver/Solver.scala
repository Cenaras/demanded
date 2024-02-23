package main.solver

import main.constraint.*

import scala.collection.mutable

trait Exhaustive extends BaseSolver {

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
   * Handles the demand part of a copy operation. That is for to = from, if ⟦to⟧ ∈ Q then add ⟦from⟧ to Q and propagate
   * tokens ⟦from⟧ ⊆ ⟦to⟧
   *
   * @param from  from constraint variable
   * @param to    to constraint variable
   * @param debug Optional constraint for debugging
   * @return if the solution state changed
   */
  def handleDemandOfCopy(from: ConstraintVar, to: ConstraintVar, debug: Option[CopyConstraint]): Boolean = {
    var changed = false
    if (Q.contains(to)) {
      changed |= addDemand(from, debug)
      changed |= propagate(from, to)
    }
    changed
  }

  /**
   * If the destination of the new constraint is tracked, place the token in its constraint variable. Similarly, if the
   * token is tracked, it must be placed in the constraint variable.
   *
   * @param c the new constraint
   * @return if the solution state was changed.
   */
  def handleNewObj(c: NewConstraint): Boolean = {
    var changed = false
    if (Q.contains(c.to)) {
      changed |= c.to.addToken(c.token)
      if changed then debug("Processing address constraint %s in %s".format(c.token, c.to))
    }
    if (W.contains(c.token)) {
      changed |= c.to.addToken(c.token)
      if changed then debug("Processing tracking in address constraint %s in %s".format(c.token, c.to))
    }
    changed
  }

  /**
   * If the function corresponding to funToken has tracked values in its return node, we must track funToken as well to
   * ensure we know where this function is called such that we can correctly propagate the tracked token in the return
   * node.
   *
   * @param funToken    the function token
   * @param constraints constraint environment
   * @param debug       optional debugging constraint
   * @return if solution state was changed
   */
  def trackFunctionIfTrackedRetNode(funToken: FunToken, constraints: ConstraintEnvironment, debug: Option[NewConstraint]): Boolean = {
    var changed = false
    val (param, ret) = constraints.funInfo(funToken)
    if (W.intersect(ret.solution).nonEmpty) {
      changed |= addTracking(funToken, debug)
    }
    changed
  }

  /**
   * Handles the demanded part of a load operation. That is, for dst = base.f, checks if ⟦dst⟧ ∈ Q, and if so adds
   * ⟦base⟧ to Q and ∀ t ∈ ⟦base⟧, add t to W, add ⟦t.f⟧ to Q and propagate ⟦t.f⟧ ⊆ ⟦dst⟧
   *
   * @param dst         destination of the read
   * @param base        base of the read
   * @param field       read field
   * @param debug       optional constraint used for debugging
   * @param constraints constraint environment
   * @return if solution state was changed
   */
  def handleDemandOfLoad(dst: ConstraintVar, base: ConstraintVar, field: String, debug: Option[ComplexConstraint], constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    if (Q.contains(dst)) {
      changed |= addDemand(base, debug)
      base.solution.foreach(t => {
        changed |= addTracking(t, debug)
        val tf = constraints.tf2Cvar((t, field))
        changed |= addDemand(tf, debug)
        changed |= dst.addTokens(tf.solution)
      })
    }
    changed
  }

  /**
   * For each token t in the base solution set, compute the set of tracked tokens of t.f and propagate to dst.
   *
   * @param base        the base
   * @param dst         the destination
   * @param field       the field f
   * @param constraints constraint environment
   * @return if the solution state was changed
   */
  def propagateTrackedOfField(base: ConstraintVar, dst: ConstraintVar, field: String, constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    base.solution.foreach(t => {
      val tf = constraints.tf2Cvar((t, field))
      val tracked = tf.solution.intersect(W)
      changed |= dst.addTokens(tracked)
    })
    changed
  }

  /**
   * If dst is demanded, demand src and propagate all tokens from src to dst
   *
   * @param dst   destination constraint variable
   * @param src   source constraint variable
   * @param debug debug information
   * @param constraints constraint environment
   * @return if solution state was changed
   */
  def demandAndPropagate(dst: ConstraintVar, src: ConstraintVar, debug: Option[Constraint], constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    if (Q.contains(dst)) {
      changed |= addDemand(src, debug)
      changed |= dst.addTokens(src.solution)
    }
    changed
  }

  /**
   *
   * @param res
   * @param callNode
   * @param debug
   * @param constraints
   * @return
   */
  def demandCallAndPropagate(res: ConstraintVar, callNode: ConstraintVar, debug: Option[ComplexConstraint], constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    if (Q.contains(res)) {
      changed |= addDemand(callNode, debug)


      callNode.solution.foreach {
        case a: ObjToken =>
        case b: FunToken =>
          val (paramNode, retNode) = constraints.funInfo(b)
          changed |= addDemand(retNode, debug)
          changed |= res.addTokens(retNode.solution)
      }
    }
    changed
  }


  def demandArgAndPropagate(callNode: ConstraintVar, arg: ConstraintVar, debug: Option[ComplexConstraint], constraints: ConstraintEnvironment): Boolean = {
    var changed = false

    callNode.solution.foreach {
      case a: ObjToken =>
      case b: FunToken =>
        val (paramNode, retNode) = constraints.funInfo(b)
        changed |= demandAndPropagate(paramNode, arg, debug, constraints)
    }
    changed
  }

  /**
   * Adds demand to a constraint and tracks all tokens in its solution
   *
   * @param cVar  the constraint variable to demand and track all tokens of
   * @param debug Optional constraint for debugging
   * @return if the solution state was changed
   */
  def demandAndTrackAll(cVar: ConstraintVar, debug: Option[ComplexConstraint]): Boolean = {
    var changed = false
    changed |= addDemand(cVar, debug)
    // We cannot move this into handleDemandOfStore - TrackBaseInStore fails then. And it is more restrictive here, than
    // always doing it.
    cVar.solution.foreach(t => {
      changed |= addTracking(t, debug)
    })
    changed
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

  /**
   * Solve a set of constraints in a demanded manner, repeatedly iterating all constraints processing demanded, and
   * adding new constraints to demanded and tracking to progress the solution. Applies until a fixpoint is reached.
   *
   * @param constraints the constraints environment to solve
   * @param queryID     id for constraint variable to initially demand.
   * @return list of solved constraint variables.
   */
  def solve(constraints: ConstraintEnvironment, queryID: QueryID): ConstraintVariables
}
