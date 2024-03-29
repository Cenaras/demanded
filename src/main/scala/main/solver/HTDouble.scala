package main.solver

import main.constraint.*

import scala.collection.mutable

class HTDouble extends Demanded {
  private val cVar2Tracking: mutable.Map[ConstraintVar, ConstraintVar] = mutable.Map()

  def solve(constraints: ConstraintEnvironment, queryID: QueryID): ConstraintVariables = {
    Q.clear()
    W.clear()

    debug("Solving HTDouble instance on query %s\n".format(queryID))
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

        c.token match
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            if (cVar2Tracking(retNode).solution.nonEmpty) {
              changed |= addTracking(c.token, Some(c))
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
          changed |= demandAndPropagate(tf, src, Some(constraint), constraints)
          changed |= cVar2Tracking(tf).addTokens(cVar2Tracking(src).solution)
        })

        if (cVar2Tracking(src).solution.nonEmpty) {
          changed |= demandAndTrackAll(base, Some(constraint))
        }


      case CallConstraint(res, callNode, arg) =>
        changed |= demandCallAndPropagate(res, callNode, Some(constraint), constraints)

        callNode.solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            changed |= cVar2Tracking(res).addTokens(cVar2Tracking(retNode).solution)
        }

        changed |= demandArgAndPropagate(callNode, arg, Some(constraint), constraints)


        if (cVar2Tracking(arg).solution.nonEmpty) {
          changed |= addDemand(callNode, Some(constraint))
        }


        callNode.solution.foreach {
          case a: ObjToken =>
          case b: FunToken =>
            val (paramNode, retNode) = constraints.funInfo(b)
            changed |= cVar2Tracking(paramNode).addTokens(cVar2Tracking(arg).solution)
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
