package main.solver

import main.constraint.*

import scala.collection.mutable

/** A query is either for a base constraint variable (denoted by varId)
 * or a field constraint variable (denoted by (tokenId, field)) */
type QueryID = Int | (Int, String)

class HTSolver extends BaseSolver {
  
  /** List of queried constraint variables. */
  private val Q: mutable.Set[ConstraintVar] = mutable.Set()
  /** List of tracked tokens. Tracked tokens must be propagated regardless of demand. */
  private val W: mutable.Set[Token] = mutable.Set()


  private def addDemand(constraintVar: ConstraintVar, constraint: Option[Constraint]): Boolean = {
    if (Q.add(constraintVar)) {
      debug("Adding %s to demand due to %s".format(constraintVar, constraint))
      return true
    }
    false
  }


  private def addTracking(token: Token, constraint: Constraint): Boolean = {
    if (W.add(token)) {
      debug("Adding %s to tracking due to %s".format(token, constraint))
      return true
    }
    false
  }


  def solve(constraints: Constraints, queryId: QueryID): ConstraintVariables = {

    val queriedCvar = queryId match
      case (t, f) =>
        W.add(constraints.id2ObjToken(t)) // FIXME: Added this rule to ensure tracking when querying field cvar
        constraints.tf2Cvar.get((constraints.id2ObjToken(t), f))

      case x: Int => constraints.id2Cvar.get(x)

    queriedCvar match
      case None => throw Error("Queried variable does not exist")
      case Some(v) => addDemand(v, None)
    var changed = true;


    // We also must iterate the address constraints in the demanded version
    while (changed) {
      changed = false

      // Address constraints are only processed if the constraint variable is queried or the token is tracked
      constraints.addrConstraints.foreach(c => {
        if (Q.contains(c.to)) {
          changed |= c.to.addToken(c.token)
          if changed then debug("Processing address constraint %s in %s".format(c.token, c.to))
        }
        // TODO: This prints also if above made changed go true and this statement actually did nothing...
        if (W.contains(c.token)) {
          changed |= c.to.addToken(c.token)
          if changed then debug("Processing tracking in address constraint %s in %s".format(c.token, c.to))
        }
      })

      // Copy constraints
      constraints.copyConstraints.foreach(c => {
        if (Q.contains(c.to)) {
          changed |= addDemand(c.from, Some(c))
          changed |= propagate(c.from, c.to)
        }
        val tracked = c.from.solution.intersect(W)
        changed |= c.to.addTokens(tracked)
      })
      constraints.complexConstraints.foreach(c => changed |= solveComplex(c, constraints))
    }
    constraints.constraintVars
  }


  private def solveComplex(constraint: ComplexConstraint, constraints: Constraints): Boolean = {
    var changed = false
    constraint match {
      case ForallLoadConstraint(dst, base, field) =>
        if (Q.contains(dst)) {
          changed |= addDemand(base, Some(constraint))
          base.solution.foreach(t => {
            changed |= addTracking(t, constraint)
          })

          base.solution.foreach(t => {
            val tf = constraints.tf2Cvar((t, field))
            changed |= addDemand(tf, Some(constraint))
            // NOTE: By adding a copy constraint we also treat adding tf to demanded if dst was, so for now try with this.
            changed |= dst.addTokens(tf.solution)
          })
        }
        base.solution.foreach(t => {
          val token = constraints.id2ObjToken(t.id)
          val cvar = constraints.tf2Cvar((token, field))
          val tracked = cvar.solution.intersect(W)
          changed |= dst.addTokens(tracked)
        })

      case ForallStoreConstraint(base, field, src) =>
        base.solution.foreach(t => {
          debug("Processing token %s for store on %s.%s = %s".format(t, base, field, src.toString))
          val tf = constraints.tf2Cvar((t, field))
          if (Q.contains(tf)) {
            changed |= addDemand(src, Some(constraint))
            // NOTE: Same applies here
            changed |= tf.addTokens(src.solution)
          }

          // I think this is supposed to be here - we cannot guard the rule satisfying the invariant, it must always hold!
          changed |= tf.addTokens(src.solution.intersect(W))

          // TODO: Is this needed?
//          tf.solution.foreach(t => {
//            changed |= addTracking(t, constraint)
//          })
        })

        if (src.solution.intersect(W).nonEmpty) {
          changed |= addDemand(base, Some(constraint))
          // FIXME: Added this rule, unsure if it is correct for minimal solution
          base.solution.foreach(t => {
            changed |= addTracking(t, constraint)
          })
        }
    }

    changed
  }

}