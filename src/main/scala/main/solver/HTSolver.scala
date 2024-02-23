package main.solver

import main.constraint.*

import scala.collection.mutable

// TODO: Disable the call stuff and make sure that all the rules for the load/store are needed
//  Provide a reasoning for all the rules so we are sure that they are relevant.
//  Then from there, reformulate the call stuff - it is basically a load and a store - remember the implication and do
//  we actually need the tracked part in the second rule? I think that maybe the call stuff is a bit wrong...


// Checklist - are the constraints implemented as in the document + can we remove any of them?
// (comment out stuff, try combinations, everytime we get a failure construct a small example from that case)
// **************************************************
// new constraints work as described in the document
// assign constraints work as described in the document
// load constraints work as described in the document
// store constraints work as described in the document

class HTSolver extends Demanded {

  def solve(constraints: ConstraintEnvironment, queryId: QueryID): ConstraintVariables = {

    var iterations = 0
    debug("Solving HTSolver instance on query %s\n".format(queryId))
    demandQuery(queryId, constraints)

    var changed = true
    while (changed) {
      iterations += 1
      changed = false

      // Address constraints are only processed if the constraint variable is queried or the token is tracked
      constraints.newConstraints.foreach(c => {
        if (Q.contains(c.to)) {
          changed |= c.to.addToken(c.token)
          if changed then debug("Processing address constraint %s in %s".format(c.token, c.to))
        }
        // TODO: This prints also if above made changed go true and this statement actually did nothing...
        if (W.contains(c.token)) {
          changed |= c.to.addToken(c.token)
          if changed then debug("Processing tracking in address constraint %s in %s".format(c.token, c.to))
        }

        // If some function definition has a return node which contains tracked tokens, we must track the function
        // to know where it ends up such that we can propagate the tracked return tokens.
        c.token match
          case a: ObjToken =>
          case b: FunToken =>
            val (param, ret) = constraints.funInfo(b)
            if (W.intersect(ret.solution).nonEmpty) {
              changed |= addTracking(c.token, Some(c))
            }


      })

      // Copy constraints
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

        base.solution.foreach(t => {
          val cvar = constraints.tf2Cvar((t, field))
          val tracked = cvar.solution.intersect(W)
          changed |= dst.addTokens(tracked)
        })

      case ForallStoreConstraint(base, field, src) =>
        base.solution.foreach(t => {
          debug("Processing token %s for store on %s.%s = %s".format(t, base, field, src.toString))
          val tf = constraints.tf2Cvar((t, field))
          changed |= handleDemandOfStore(tf, src, Some(constraint), constraints)

          // I think this is supposed to be here - we cannot guard the rule satisfying the invariant, it must always hold!
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
        if (Q.contains(res)) {
          changed |= addDemand(callNode, Some(constraint))
          callNode.solution.foreach {
            case a: ObjToken =>
            case b: FunToken =>
              val (paramNode, retNode) = constraints.funInfo(b)
              changed |= addDemand(retNode, Some(constraint))
              changed |= res.addTokens(retNode.solution)
          }
        }
        // Tracked tokens in return node must be propagated as well.
        callNode.solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            changed |= res.addTokens(retNode.solution.intersect(W))
        }

        /** Store part of the call expression - storing the argument into the parameter */
        // If the parameter is demanded, we must propagate the argument into it.
        callNode.solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            if (Q.contains(paramNode)) {
              changed |= addDemand(arg, Some(constraint))
              changed |= paramNode.addTokens(arg.solution)
            }
        }

        // TODO: URGENT! There are some fixes we need to make. Every function parameter should be unique - 
        //  but we should still allow the body to take that value
        //  That is: If we have n variables, we should still generate function parameters from those n values, but
        //  just say that whenever we generate a parameter it should be unique, and whenever we generate a random variable,
        //  it should not be allowed to have the same id as an existing parameter. Then we can just let bodies be
        //  completely random!

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