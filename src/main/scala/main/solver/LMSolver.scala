package main.solver

import main.constraint.*

/** Alternative solver formulation. Does not support function calls. */
class LMSolver extends Demanded {

  override def solve(constraints: ConstraintEnvironment, queryID: QueryID): ConstraintVariables = {
    debug("Solving LMSolver instance on query %s\n".format(queryID))
    demandQuery(queryID, constraints)

    var iterations = 0
    var changed = true
    while (changed) {
      iterations += 1
      changed = false

      /** General rules */
      Q.foreach {
        case BaseConstraintVar(id) =>
        case FieldConstraintVar(token, field) => changed |= addTracking(token, None)
      }

      constraints.constraintVars.foreach {
        case BaseConstraintVar(id) =>
        case FieldConstraintVar(token, field) =>
          val tf = constraints.tf2Cvar((token, field))
          if (tf.solution.intersect(W).nonEmpty) {
            changed |= addTracking(token, None)
          }
      }


      /** Identical to HTSolver */
      constraints.newConstraints.foreach(c => {
        changed |= handleNewObj(c)
      })

      /** Identical to HTSolver */
      constraints.copyConstraints.foreach(c => {
        changed |= handleDemandOfCopy(c.from, c.to, Some(c))
        changed |= c.to.addTokens(c.from.solution.intersect(W))
      })


      constraints.complexConstraints.foreach(c => changed |= solveComplex(c, constraints))

    }

    debug("LM Iterations: " + iterations)
    constraints.constraintVars

  }


  private def solveComplex(constraint: ComplexConstraint, constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    constraint match {
      case ForallLoadConstraint(dst, base, field) =>
        base.solution.foreach(t => {
          if (Q.contains(dst)) {
            val tf = constraints.tf2Cvar((t, field))
            changed |= dst.addTokens(tf.solution)
            changed |= addDemand(tf, Some(constraint))
          }
        })

        changed |= propagateTrackedOfField(base, dst, field, constraints)

        if (Q.contains(dst)) {
          changed |= addDemand(base, Some(constraint))
        }

      case ForallStoreConstraint(base, field, src) =>
        base.solution.foreach(t => {
          val tf = constraints.tf2Cvar((t, field))
          if (Q.contains(tf)) {
            changed |= tf.addTokens(src.solution)
            changed |= addDemand(src, Some(constraint))
          }
        })
        base.solution.foreach(t => {
          val tf = constraints.tf2Cvar((t, field))
          changed |= tf.addTokens(src.solution.intersect(W))
        })
        if (src.solution.intersect(W).nonEmpty) {
          changed |= addDemand(base, Some(constraint))
        }


      case _ => ???
    }
    changed
  }

}
