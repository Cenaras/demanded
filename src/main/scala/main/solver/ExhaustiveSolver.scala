package main.solver

import main.constraint.*

import scala.collection.mutable

class ExhaustiveSolver extends BaseSolver {

  def solve(constraints: ConstraintEnvironment): ConstraintVariables = {
    constraints.newConstraints.foreach(c => {
      c.to.addToken(c.token)
    })

    var changed = true

    while (changed) {
      changed = false

      // Process copy constraints
      constraints.copyConstraints.foreach(c => {
        changed |= propagate(c.from, c.to)
      })

      // Process complex constraints, adding new copy constraints
      constraints.complexConstraints.foreach(c => {
        changed |= solveComplex(c, constraints)
      })
    }
    constraints.constraintVars
  }


  private def solveComplex(constraint: ComplexConstraint, constraints: ConstraintEnvironment): Boolean = {
    var changed = false
    constraint match
      case ForallLoadConstraint(dst, base, field) => base.solution.foreach(t => {
        val cvar = constraints.tf2Cvar(t, field)
        changed |= dst.addTokens(cvar.solution)
      })
      case ForallStoreConstraint(base, field, src) => base.solution.foreach(t => {
        val cvar = constraints.tf2Cvar(t, field)
        changed |= cvar.addTokens(src.solution)
      })
      case CallConstraint(res, call, arg) => call.solution.foreach {
        case f: FunToken =>
          val (param, ret) = constraints.funInfo(f)
          changed |= param.addTokens(arg.solution)
          changed |= res.addTokens(ret.solution)
        case _ =>
      }

    changed
  }


}
