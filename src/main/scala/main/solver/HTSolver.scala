package main.solver

import main.constraint.*

import scala.collection.mutable

class HTSolver extends Demanded {

  def solve(constraints: ConstraintEnvironment, queryId: QueryID): ConstraintVariables = {

    var iterations = 0
    debug("Solving HTSolver instance on query %s\n".format(queryId))
    demandQuery(queryId, constraints)

    var changed = true
    while (changed) {
      iterations += 1
      changed = false

      constraints.newConstraints.foreach(c => {
        changed |= handleNewObj(c)

        c.token match
          case a: ObjToken =>
          case b: FunToken =>
            changed |= trackFunctionIfTrackedRetNode(b, constraints, Some(c))
      })

      constraints.copyConstraints.foreach(c => {
        changed |= handleDemandOfCopy(c.from, c.to, Some(c))
        val tracked = c.from.solution.intersect(W)
        changed |= c.to.addTokens(tracked)
      })
      constraints.complexConstraints.foreach(c => changed |= solveComplex(c, constraints))
    }

    debug("HT Iterations: " + iterations)
    constraints.constraintVars
  }


  private def solveComplex(constraint: ComplexConstraint, constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    constraint match {
      case ForallLoadConstraint(dst, base, field) =>
        changed |= handleDemandOfLoad(dst, base, field, Some(constraint), constraints)
        changed |= propagateTrackedOfField(base, dst, field, constraints)

      case ForallStoreConstraint(base, field, src) =>
        base.solution.foreach(t => {
          debug("Processing token %s for store on %s.%s = %s".format(t, base, field, src.toString))
          val tf = constraints.tf2Cvar((t, field))
          changed |= demandAndPropagate(tf, src, Some(constraint), constraints)

          // Always propagate tracked tokens from src
          changed |= tf.addTokens(src.solution.intersect(W))
        })

        if (src.solution.intersect(W).nonEmpty) {
          changed |= demandAndTrackAll(base, Some(constraint))
        }

      case CallConstraint(res, callNode, arg) =>
        // A call expression can be viewed as a combination of a load and a store. We load the result from t.ret into
        // x, and we store the value of z into t.param.

        /** Load part of the call expression - loading the result value into x. */
        // Handling explicit demand
        changed |= demandCallAndPropagate(res, callNode, Some(constraint), constraints)

        // Tracked tokens in return node must be propagated as well.
        callNode.solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            changed |= res.addTokens(retNode.solution.intersect(W))
        }

        /** Store part of the call expression - storing the argument into the parameter */
        // If the parameter is demanded, we must propagate the argument into it.
        changed |= demandArgAndPropagate(callNode, arg, Some(constraint), constraints)

        // If the argument holds tracked tokens, we must find all possible functions and propagate the tracked tokens
        // into t.p
        if (W.intersect(arg.solution).nonEmpty) {
          changed |= addDemand(callNode, Some(constraint))

        }
        callNode.solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            changed |= paramNode.addTokens(arg.solution.intersect(W))
        }
    }
    changed
  }

}