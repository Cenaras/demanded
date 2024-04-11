package main.util

import main.program.{AssignInsn, CallInsn, LoadInsn, NewInsn, Program, StoreInsn}

import java.io.PrintWriter

object DatalogCompiler {

  var datalogDir: String = "untitled/src/datalog/"

  /**
   * Compiles a program into the expected datalog format. The compiled output is directly written to the src/datalog/
   * directory in the corresponding .input files
   * @param program the program to compile
   */
  def compile(program: Program): Unit = {

    // Clear contents of files
    val newWriter = new PrintWriter(datalogDir + "new.facts")
    val assignWriter = new PrintWriter(datalogDir + "assign.facts")
    val loadWriter = new PrintWriter(datalogDir + "load.facts")
    val storeWriter = new PrintWriter(datalogDir + "store.facts")

    program.getInstructions.foreach {
      case NewInsn(varId, tokenId) => newWriter.write("x%s\tt%s\n".format(varId, tokenId))
      case AssignInsn(left, right) => assignWriter.write("x%s\tx%s\n".format(left, right))
      case LoadInsn(left, right, field) => loadWriter.write("x%s\tx%s\t%s\n".format(left, right, field))
      case StoreInsn(left, field, right) => storeWriter.write("x%s\t%s\tx%s\n".format(left, field, right))
      case _ => throw new Error("Functions are unsupported in the logic analysis")
    }

    newWriter.close()
    assignWriter.close()
    loadWriter.close()
    storeWriter.close()
  }


}
