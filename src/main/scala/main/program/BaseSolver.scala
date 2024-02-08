package main.program

import main.constraint.{ConstraintVar, ConstraintVariables}

class BaseSolver {
  private val DEBUG = true

  protected def propagate(from: ConstraintVar, to: ConstraintVar): Boolean = {
    var changed = false
    changed |= to.addTokens(from.solution)
    if changed then debug("Processing propagation from %s to %s".format(from, to))
    changed
  }

  protected def debug(msg: String): Unit = {
    if (DEBUG) then println(msg)
  }
}

object SolverUtil {
  /**
   * Accumulates the amount of tokens present in each constraint variable in the solution
   */
  def solutionSize(solution: ConstraintVariables): Int = {
    var size = 0
    solution.foreach(c => {
      size += c.solution.size
    })
    size
  }
}
