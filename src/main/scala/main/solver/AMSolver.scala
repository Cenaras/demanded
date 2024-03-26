package main.solver

import main.constraint.{CallConstraint, ComplexConstraint, ConstraintEnvironment, ConstraintVariables, ForallLoadConstraint, ForallStoreConstraint, FunToken, ObjToken}

/** Slight alteration of formulation by Anders. */
// TODO: Try and add the missing thing for function definitions, and check if these formulations are "as minimal" as HT.
class AMSolver extends Demanded {

  def solve(constraints: ConstraintEnvironment, queryId: QueryID): ConstraintVariables = {
    var changed = true
    Q.clear()
    W.clear()
    demandQuery(queryId, constraints)

    while (changed) {
      changed = false

      // TODO: Implement a one-set solver and a two-set solver and place this inside those. A strategy pattern
      constraints.newConstraints.foreach(c => {
        changed |= handleNewObj(c)
      })

      constraints.copyConstraints.foreach(c => {
        changed |= handleDemandOfCopy(c.from, c.to, Some(c))
        changed |= c.to.addTokens(c.from.solution.intersect(W))
      })

      constraints.complexConstraints.foreach(c => changed |= solveComplex(c, constraints))
    }

    constraints.constraintVars
  }


  // TODO: Code duplication bad!
  private def solveComplex(constraint: ComplexConstraint, constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    constraint match {
      case ForallLoadConstraint(dst, base, field) =>
        changed |= handleDemandOfLoad(dst, base, field, Some(constraint), constraints)
        changed |= propagateTrackedOfField(base, dst, field, constraints)

      case ForallStoreConstraint(base, field, src) =>
        base.solution.foreach(t => {
          val tf = constraints.tf2Cvar((t, field))
          changed |= demandAndPropagate(tf, src, Some(constraint), constraints)
          changed |= tf.addTokens(src.solution.intersect(W))
        })

        if (src.solution.intersect(W).nonEmpty) {
          changed |= demandAndTrackAll(base, Some(constraint))
        }


      case CallConstraint(res, callNode, arg) =>
        changed |= demandArgAndPropagate(callNode, arg, Some(constraint), constraints)

        // First and last rule from Anders formulation under same quantifier
        callNode.solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            changed |= paramNode.addTokens(arg.solution.intersect(W))
            changed |= res.addTokens(retNode.solution.intersect(W))
        }

        if (arg.solution.intersect(W).nonEmpty) {
          changed |= addDemand(callNode, Some(constraint))
          // Anders change
          changed |= trackAll(callNode, Some(constraint))
        }

        changed |= demandCallAndPropagate(res, callNode, Some(constraint), constraints)

        // Anders change
        if (Q.contains(res)) {
          changed |= trackAll(callNode, Some(constraint))
        }


    }

    changed
  }

}
