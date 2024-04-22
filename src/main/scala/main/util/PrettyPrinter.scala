package main.util

import main.constraint.{BaseConstraintVar, ConstraintVar, ConstraintVariables, FieldConstraintVar}
import main.program.Program

import java.io.FileWriter
import scala.collection.mutable

object PrettyPrinter {

  def stringifySolution(solution: mutable.Set[ConstraintVar]): String = {
    var pretty = ""


    solution.foreach(f => {
      if (f.solution.nonEmpty) {
        pretty += f.toString + ": "
        f.solution.foreach(t => {
          pretty += t.toString + ", "
        })
        pretty += "\n"
      }
    })
    pretty
  }

  def stringifyProgram(program: Program): String = {
    var pretty = ""
    program.getInstructions.foreach(f => pretty += f.print() + "\n")
    pretty
  }

  def outputTSV(solution: ConstraintVariables, outfile: String): Unit = {

    val builder = new StringBuilder()
    val writer = new FileWriter(outfile)


    solution.foreach {
      case c@BaseConstraintVar(_) =>
        c.solution.foreach(t => {
          builder.append("%s\t%s\n".format(c.name, t))
        })
      case c@FieldConstraintVar(token, field) =>
        c.solution.foreach(t => {
          builder.append("%s\t%s\t%s\n".format(token, field, t))
        })
      case _ => throw new Error("Two-set implementation unsupported in Logic Analysis")
    }

    val sorted = builder.toString().linesIterator.toList.sorted.mkString("\n")
    writer.write(sorted)
    writer.close()
  }

}