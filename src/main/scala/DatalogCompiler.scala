import java.io.File
import scala.language.{existentials, postfixOps}
import sys.process.*
import java.io.{File, FileWriter, PrintWriter}

object DatalogCompiler {
  var datalogDir: String = "untitled/src/datalog/"

  /**
   * Compiles a program into the expected datalog format. The compiled output is directly written to the src/datalog/
   * directory in the corresponding .input files
   *
   * @param program the program to compile
   */
  def compile(program: Program): Unit = {

    // Clear contents of files
    val newWriter = new PrintWriter(datalogDir + "new.facts")
    val assignWriter = new PrintWriter(datalogDir + "assign.facts")
    val loadWriter = new PrintWriter(datalogDir + "load.facts")
    val storeWriter = new PrintWriter(datalogDir + "store.facts")

    program.getInstructions.foreach {
      case New(x, t) => newWriter.write("x%s\tt%s\n".format(x, t))
      case Assign(x, y) => assignWriter.write("x%s\tx%s\n".format(x, y))
      case Load(x, y, f) => loadWriter.write("x%s\tx%s\tf%s\n".format(x, y, f))
      case Store(x, f, y) => storeWriter.write("x%s\tf%s\tx%s\n".format(x, f, y))
      case _ => throw new Error("Functions are unsupported in the logic analysis")
    }

    newWriter.close()
    assignWriter.close()
    loadWriter.close()
    storeWriter.close()
  }

  /**
   * Compiles the given input program into a datalog variant, transforms the analysis into a demanded version according
   * to the provided query and runs the Soufflé datalog solver.
   * Output is written to the src/datalog directory, which is also where the .input directives expect input files to
   * reside.
   * */
  def compileAndAnalyze(p: Program, query: Cell): Unit = {
    compile(p)

    val datalogDir = "./untitled/src/datalog/"
    val scriptPath = datalogDir + "transform_program.sh"
    val exhaustivePath = datalogDir + "exhaustive.dl"
    val demandPath = datalogDir + "demand.dl"

    val cmd = "%s x%s %s %s".format(scriptPath, query, exhaustivePath, demandPath)
    cmd !!

    val analysisCmd = "souffle -F %s -D %s %s".format(datalogDir, datalogDir, demandPath)
    analysisCmd !!
  }

  /**
   * Reads the solution from the datalog files and concatenates and de-dupliucates entries and outputs a single file.
   *
   * @param outfile de-duplicated file containing all relations
   */
  def solutionToSingleTSV(outfile: String): Unit = {
    val ptbb = datalogDir + "pointsTo_bb.csv"
    val ptbf = datalogDir + "pointsTo_bf.csv"
    val ptfbbb = datalogDir + "pointsToField_bbb.csv"
    val ptfbbf = datalogDir + "pointsToField_bbf.csv"

    {
      ("sort -u %s %s %s %s".format(ptbb, ptbf, ptfbbb, ptfbbf) #> new File(outfile)).!
    }
  }

  /**
   * Collects the contents of all files that contains demanded magic sets
   */
  def collectDemand(): String = {
    val builder = new StringBuilder()

    val magic_ptbf = datalogDir + "magic_pointsTo_bf.csv"
    val magic_ptfbbf = datalogDir + "magic_pointsToField_bbf.csv"

    builder.append(FileManager.readFile(magic_ptbf)).append("\n")
    builder.append(FileManager.readFile(magic_ptfbbf))

    // De-duplicate entries if needed and sort
    builder.toString().linesIterator.toSet.mkString("\n").linesIterator.toList.sorted.mkString("\n")
  }

  /***
   * Collects the contents of all files that contain tracked tokens
   * @return newline separated string of all tracked information
   */
  def collectTracked(): String = {
    val builder = new StringBuilder()

    val magic_ptbb = datalogDir + "magic_pointsTo_bb.csv"
    val magic_ptfbbb = datalogDir + "magic_pointsToField_bbb.csv"
    builder.append(FileManager.readFile(magic_ptbb)).append("\n")
    builder.append(FileManager.readFile(magic_ptfbbb)).append("\n")

    // Only keep the token from the relation and de-duplicate
    val deduplicated = builder.toString().linesIterator.toList.map(s => s.substring(s.lastIndexOf("\t") + 1)).mkString("\n").linesIterator.toSet.mkString("\n")
    deduplicated.linesIterator.toList.sorted.mkString("\n")
  }

}
