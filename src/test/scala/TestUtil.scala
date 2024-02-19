import main.constraint.{ConstraintGenerator, ConstraintVariables}
import main.program.Program
import main.solver.{Demanded, QueryID}

object TestUtil {
  def solve(p: Program, queryId: QueryID, solver: Demanded): ConstraintVariables = {
    val constraints = ConstraintGenerator.generate(p)
    solver.solve(constraints, queryId)
  }

}
