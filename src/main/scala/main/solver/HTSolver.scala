package main.solver

import main.constraint.*

import scala.collection.mutable


class HTSolver extends Demanded {

  def solve(constraints: ConstraintEnvironment, queryId: QueryID): ConstraintVariables = {

    demandQuery(queryId, constraints)

    var changed = true
    // We also must iterate the address constraints in the demanded version
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


  private def solveComplex(constraint: ComplexConstraint, constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    constraint match {
      case ForallLoadConstraint(dst, base, field) =>
        if (Q.contains(dst)) {
          changed |= addDemand(base, Some(constraint))
          base.solution.foreach(t => {
            changed |= addTracking(t, Some(constraint))
          })

          base.solution.foreach(t => {
            val tf = constraints.tf2Cvar((t, field))
            changed |= addDemand(tf, Some(constraint))
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
          debug("Processing token %s for store on %s.%s = %s".format(t, base, field, src.toString))
          val tf = constraints.tf2Cvar((t, field))
          if (Q.contains(tf)) {
            changed |= addDemand(src, Some(constraint))
            // NOTE: Same applies here
            changed |= tf.addTokens(src.solution)
          }

          // I think this is supposed to be here - we cannot guard the rule satisfying the invariant, it must always hold!
          changed |= tf.addTokens(src.solution.intersect(W))
        })

        if (src.solution.intersect(W).nonEmpty) {
          changed |= addDemand(base, Some(constraint))
          // FIXME: Added this rule, unsure if it is correct for minimal solution
          base.solution.foreach(t => {
            changed |= addTracking(t, Some(constraint))
          })
        }

      // If result is demanded, demand call to retrieve every function and demand all return nodes.
      // For now, since every function is identity, we also demand the argument <-- FIXME
      case CallConstraint(res, callNode, arg) =>

        def propArgAndReturn(funToken: FunToken, resNode: ConstraintVar, argNode: ConstraintVar): Boolean = {
          var changed = false
          // FIXME: This might be a bit too naive
          val (argNode, resNode) = constraints.funInfo(funToken)
          changed |= addDemand(resNode, Some(constraint))
          changed |= addDemand(arg, Some(constraint))
          changed |= argNode.addTokens(arg.solution)
          changed |= res.addTokens(resNode.solution)
          changed
        }

        if (Q.contains(res)) {
          changed |= addDemand(callNode, Some(constraint))
          callNode.solution.foreach {
            case a: ObjToken =>
            case b: FunToken =>
              changed |= propArgAndReturn(b, res, arg)
              // Since arguments are merged, we must track all function tokens we see, to ensure their arguments
              // flow to the parameters in other call instances
              changed |= addTracking(b, Some(constraint))
          }
        }

        // If a call is tracked, we must copy the call site argument to the formal parameter
        callNode.solution.intersect(W).foreach {
          case a: ObjToken =>
          case b: FunToken =>
            changed |= propArgAndReturn(b, res, arg)
        }
    }
    changed
  }

}