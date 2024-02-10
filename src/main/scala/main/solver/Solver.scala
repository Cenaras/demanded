package main.solver

import main.constraint.Constraints


// TODO: Consider a structure like the following:
//  Main Solver trait which has the mappings from ids to cvars
//  Two sub-traits, one for exhaustive solvers, one for demanded solvers
//  Those subtraits have a unique solve signature

trait Solver {
  // Common functionality, i.e. mappings from ids goes here - maybe an init function? And use a return type from that as input to the solve function for clean code
}

trait ExhaustiveSolver extends Solver {

  def solve(constraints: Constraints): Unit = {
    ???
  }
}

trait DemandedSolver extends Solver {
  def solve(constraints: Constraints, queryID: QueryID): Unit = {
    ???
  }
}
