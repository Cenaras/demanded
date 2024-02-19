package main.solver

import main.constraint.{ConstraintEnvironment, ConstraintVar, ConstraintVariables}

import scala.collection.mutable

// TODO: Reuse code from HTSolver?
class HTDouble extends Demanded {


  private val cvarToTrackCvar: mutable.Map[ConstraintVar, ConstraintVar] = mutable.Map()

  def solve(constraints: ConstraintEnvironment, queryID: QueryID): ConstraintVariables = {
    generateTrackedConstraintVars(constraints.constraintVars)
    ???
  }

  private def generateTrackedConstraintVars(constraints: ConstraintVariables): Unit = {
    constraints.foreach(f => {


    })
  }


}
