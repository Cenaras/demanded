package main.solver

import main.constraint.*

import scala.collection.mutable

// TODO: Reuse code from HTSolver?
class HTDouble extends Demanded {
  private val cVar2Tracking: mutable.Map[ConstraintVar, ConstraintVar] = mutable.Map()

  // TODO: There might end up being some code duplication that we can refactor between HTSolve and this, since
  //  the rules are basically the same

  // We could make a strategy pattern where we have a strategy for demanded and a strategy for tracking
  // Then we reuse the demanded in both and only alter the tracking
  def solve(constraints: ConstraintEnvironment, queryID: QueryID): ConstraintVariables = {
    generateTrackedConstraintVars(constraints.constraintVars)
    demandQuery(queryID, constraints)

    var changed = true
    while (changed) {
      changed = false

      constraints.newConstraints.foreach(c => {
        if (Q.contains(c.to)) {
          changed |= c.to.addToken(c.token)
          if changed then debug("Processed address constraint %s in %s".format(c.token, c.to))
        }
        if (W.contains(c.token)) {
          changed |= cVar2Tracking(c.to).addToken(c.token)
          if changed then debug("Processing tracking in address constraint %s in %s".format(c.token, c.to))
        }
      })

      constraints.copyConstraints.foreach(c => {
        changed |= handleDemandOfCopy(c.from, c.to, Some(c))
        changed |= cVar2Tracking(c.to).addTokens(cVar2Tracking(c.from).solution)
      })

      constraints.complexConstraints.foreach(c => changed |= solveComplex(c, constraints))
    }

    constraints.constraintVars

  }

  private def solveComplex(constraint: ComplexConstraint, constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    constraint match
      case ForallLoadConstraint(dst, base, field) =>
        changed |= handleDemandOfLoad(dst, base, field, Some(constraint), constraints)

        base.solution.foreach(t => {
          val trackedTf = cVar2Tracking(constraints.tf2Cvar((constraints.id2Token(t.id), field)))
          val trackedDst = cVar2Tracking(dst)
          changed |= trackedDst.addTokens(trackedTf.solution)
        })

      case ForallStoreConstraint(base, field, src) =>
        base.solution.foreach(t => {
          val tf = constraints.tf2Cvar((t, field))
          changed |= handleDemandOfStore(tf, src, Some(constraint), constraints)
          changed |= cVar2Tracking(tf).addTokens(cVar2Tracking(src).solution)
        })

        if (cVar2Tracking(src).solution.nonEmpty) {
          changed |= demandAndTrackAll(base, Some(constraint))
        }


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
              changed |= addTracking(b, Some(constraint))
          }
        }

        cVar2Tracking(callNode).solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            changed |= propArgAndReturn(b, res, arg)
        }


    changed
  }

  private def generateTrackedConstraintVars(constraintVariables: ConstraintVariables): Unit = {
    constraintVariables.foreach(f => {
      val tracked = f match {
        case a: BaseConstraintVar => TrackedBaseConstraintVar(a.getId, a)
        case b: FieldConstraintVar => TrackedFieldConstraintVar(b.getToken, b.getField, b)
      }
      cVar2Tracking += f -> tracked
    })
  }


}
