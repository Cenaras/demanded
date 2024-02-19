package main.solver

import main.constraint.{ConstraintEnvironment, ConstraintVariables}

trait Exhaustive {

}

trait Demanded {


  def solve(constraints: ConstraintEnvironment, queryID: QueryID): ConstraintVariables
}
