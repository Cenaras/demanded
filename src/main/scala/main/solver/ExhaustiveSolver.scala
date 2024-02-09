package main.solver

import main.constraint.*

import scala.collection.mutable

class ExhaustiveSolver extends BaseSolver {

  def solve(constraints: Constraints): ConstraintVariables = {
    constraints.addrConstraints.foreach(c => {
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


  private def solveComplex(constraint: ComplexConstraint, constraints: Constraints): Boolean = {
    var changed = false
    constraint match
      case ForallLoadConstraint(dst, base, field) => base.solution.foreach(t => {
        val cvar = constraints.tf2Cvar((t, field))
        changed |= dst.addTokens(cvar.solution)
      })
      case ForallStoreConstraint(base, field, src) => base.solution.foreach(t => {
        val cvar = constraints.tf2Cvar((t, field))
        changed |= cvar.addTokens(src.solution)
      })
    changed
  }


}
