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

    debug("Solving HTSolver instance on query %s\n".format(queryId))
    demandQuery(queryId, constraints)

    var changed = true
    while (changed) {
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
      })

      // Copy constraints
      constraints.copyConstraints.foreach(c => {
        changed |= handleDemandOfCopy(c.from, c.to, Some(c))
        val tracked = c.from.solution.intersect(W)
        changed |= c.to.addTokens(tracked)
      })
      constraints.complexConstraints.foreach(c => changed |= solveComplex(c, constraints))
    }
    //    println(W)
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

          // FIXME: This would be less restrictive than only tracking tokens from base, if src n W is non-empty
          // changed |= addTracking(t, None)
        })

        if (src.solution.intersect(W).nonEmpty) {
          // TODO: Added this rule, unsure if it is correct for minimal solution
          changed |= demandAndTrackAll(base, Some(constraint))
        }

      // If result is demanded, demand call to retrieve every function and demand all return nodes.
      // For now, since every function is identity, we also demand the argument <-- FIXME
      case CallConstraint(res, callNode, arg) =>

        // A call expression can be viewed as a combination of a load and a store. We load the result from t.ret into
        // x, and we store the value of z into t.param.

        /** Load part of the call expression - loading the result value into x. */
        // Handling explicit demand
        if (Q.contains(res)) {
          changed |= addDemand(callNode, Some(constraint))
          // TODO: Track y?
          callNode.solution.foreach {
            case a: ObjToken =>
            case b: FunToken =>
              val (paramNode, retNode) = constraints.funInfo(b)
              changed |= addDemand(retNode, Some(constraint))
              res.addTokens(retNode.solution)
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

        // TODO: Add test cases that justify these rules - are they even needed?

        // If the argument holds tracked tokens, we must find all possible functions and propagate the tracked tokens
        // into t.p
        if (W.intersect(arg.solution).nonEmpty) {
          changed |= addDemand(callNode, Some(constraint))

          callNode.solution.foreach {
            case a: ObjToken =>
            case b: FunToken =>
              // FIXME: Is this needed?
              changed |= addTracking(b, Some(constraint))
          }
        }
        callNode.solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            changed |= paramNode.addTokens(arg.solution.intersect(W))
        }









      //        // TODO: Right now, we are obeying the exhaustive solution, by merging all arguments for tracked functions.
      //        //  However one could argue that maybe we can avoid doing that and thereby gain some form of precision, i.e.
      //        //  for x = y(a), z = y(b) where x is demanded, if z is not demanded then we don't merge a, b, giving more
      //        //  precision for x
      //
      //        def propArgAndReturn(funToken: FunToken, resNode: ConstraintVar, argNode: ConstraintVar): Boolean = {
      //          var changed = false
      //          // FIXME: This might be a bit too naive
      //          val (argNode, resNode) = constraints.funInfo(funToken)
      //          debug("Propagating argument %s into formal param %s and return node %s into dst %s".format(arg, argNode, resNode, res))
      //          changed |= addDemand(resNode, Some(constraint))
      //          changed |= addDemand(arg, Some(constraint))
      //          changed |= argNode.addTokens(arg.solution)
      //          changed |= res.addTokens(resNode.solution)
      //          changed
      //        }
      //
      //        if (Q.contains(res)) {
      //          changed |= addDemand(callNode, Some(constraint))
      //          callNode.solution.foreach {
      //            case a: ObjToken =>
      //            case b: FunToken =>
      //              changed |= propArgAndReturn(b, res, arg)
      //              // Since arguments are merged, we must track all function tokens we see, to ensure their arguments
      //              // flow to the parameters in other call instances
      //              changed |= addTracking(b, Some(constraint))
      //          }
      //        }
      //
      //        // If a call is tracked, we must copy the call site argument to the formal parameter
      //        callNode.solution.intersect(W).foreach {
      //          case a: ObjToken =>
      //          case b: FunToken =>
      //            changed |= propArgAndReturn(b, res, arg)
      //        }
    }
    changed
  }

}