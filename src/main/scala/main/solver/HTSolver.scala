package main.solver

import main.constraint.*

import scala.collection.mutable


class HTSolver {


  val Q: mutable.Set[ConstraintVar] = mutable.Set()
  val W: mutable.Set[Token] = mutable.Set()

  val DEBUG = false;

  private def addDemand(constraintVar: ConstraintVar): Boolean = {
    if (Q.add(constraintVar)) {
      debug("Adding %s to demand".format(constraintVar))
      return true
    }
    false
  }


  private def addTracking(token: Token): Boolean = {
    if (W.add(token)) {
      debug("Adding %s to tracking".format(token))
      return true
    }
    false

  }

  private def debug(msg: String): Unit = {
    if (DEBUG) then println(msg)
  }

  /** A query is either for a base constraint variable (denoted by varId)
   * or a field constraint variable (denoted by (tokenId, field)) */
  private type QueryID = Int | (Int, String)

  def solve(constraints: Constraints, queryId: QueryID): ConstraintVariables = {

    val queriedCvar = queryId match
      case (t, f) => constraints.tf2Cvar.get((constraints.id2Token(t), f))
      case x: Int => constraints.id2Cvar.get(x)

    queriedCvar match
      case None => throw Error("Queried variable does not exist")
      case Some(v) => addDemand(v)
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

        if (W.contains(c.token)) {
          changed |= c.to.addToken(c.token)
          if changed then debug("Processing tracking in address constraint %s in %s".format(c.token, c.to))
        }
      })

      // Copy constraints
      constraints.copyConstraints.foreach(c => {
        if (Q.contains(c.to)) {
          changed |= addDemand(c.from)
          changed |= propagate(c.from, c.to)
        }
        val tracked = c.from.solution.intersect(W)
        c.to.addTokens(tracked)
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
          changed |= addDemand(base)
          base.solution.foreach(t => {
            changed |= addTracking(t)
          })

          base.solution.foreach(t => {
            val tf = constraints.tf2Cvar((t, field))
            changed |= addDemand(tf)
            // NOTE: By adding a copy constraint we also treat adding tf to demanded if dst was, so for now try with this.
            changed |= dst.addTokens(tf.solution)
          })
        }
        base.solution.foreach(t => {
          val token = constraints.id2Token(t.id)
          val cvar = constraints.tf2Cvar((token, field))
          val tracked = cvar.solution.intersect(W)
          changed |= dst.addTokens(tracked)
        })

      case ForallStoreConstraint(base, field, src) =>
        base.solution.foreach(t => {
          val tf = constraints.tf2Cvar((t, field))
          if (Q.contains(tf)) {
            changed |= addDemand(src)
            // NOTE: Same applies here
            changed |= tf.addTokens(src.solution)
            changed |= tf.addTokens(src.solution.intersect(W))

            // TODO: Is this actually needed?
            tf.solution.foreach(t => {
              changed |= addTracking(t)
            })
          }
        })

        // TODO: Is this actually needed?
        if (src.solution.intersect(W).nonEmpty) {
          changed |= addDemand(base)
        }
    }

    changed
  }


  private def propagate(from: ConstraintVar, to: ConstraintVar): Boolean = {
    var changed = false
    val fromTokens = from.solution
    fromTokens.foreach(t => {
      changed |= to.addToken(t)
    })
    if changed then debug("Processing propagation from %s to %s".format(from, to))
    changed
  }
}