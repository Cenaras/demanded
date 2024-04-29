import java.io.File
import scala.language.{existentials, postfixOps}
import sys.process.*
import java.io.{File, FileWriter, PrintWriter}


trait DatalogAnalysis {

  val datalogDir: String = "untitled/src/datalog/"

  val scriptPath: String = datalogDir + "transform_program.sh"
  val ptbb = datalogDir + "pointsTo_bb.csv"
  val ptbf = datalogDir + "pointsTo_bf.csv"
  val ptfbbb = datalogDir + "pointsToField_bbb.csv"
  val ptfbbf = datalogDir + "pointsToField_bbf.csv"

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

  private def analyze(query: Cell): Unit = {
    val cmd = "%s x%s %s %s".format(scriptPath, query, exhaustivePath(), demandedPath())
    cmd !!

    val analysisCmd = "souffle -F %s -D %s %s".format(datalogDir, datalogDir, demandedPath())
    analysisCmd !!
  }

  def compileAndAnalyze(p: Program, query: Cell): Unit = {
    compile(p)
    analyze(query)
  }


  // Configurations
  def exhaustivePath(): String
  def demandedPath(): String

  def demandFiles(): List[String]
  def trackFiles(): List[String]

  def outputSolution(outfile: String): Unit

  def collectDemand(): String = {
    val builder = StringBuilder()
    for f <- demandFiles() do
      builder.append(FileManager.readFile(datalogDir + f)).append("\n")

    builder.toString().linesIterator.toSet.mkString("\n").linesIterator.toList.sorted.mkString("\n")
  }
  def collectTracked(): String = {
    val builder = StringBuilder()
    for f <- trackFiles() do
      builder.append(FileManager.readFile(datalogDir + f)).append("\n")

    val deduplicated = builder.toString().linesIterator.toList.mkString("\n").linesIterator.toSet.mkString("\n")
    deduplicated.linesIterator.toList.sorted.mkString("\n")
  }
  
  private def readSolution(path: String): String = {
    FileManager.readFile(path)
  }

  def cost(path: String): Int = {
    val demand = collectDemand()
    val tracked = collectTracked()
    val solution = readSolution(path)
    
    
    demand.linesIterator.size + tracked.linesIterator.size + solution.linesIterator.size
  }

}

class Standard extends DatalogAnalysis {
  override def exhaustivePath(): String = datalogDir + "exhaustive.dl"
  override def demandedPath(): String = datalogDir + "demand.dl"

  override def demandFiles(): List[String] = List("magic_pointsTo_bf.csv", "magic_pointsToField_bbf.csv")
  override def trackFiles(): List[String] = List("magic_pointsTo_bb.csv", "magic_pointsToField_bbb.csv")

  override def outputSolution(outfile: String): Unit = {
    ("sort -u %s %s %s %s".format(ptbb, ptbf, ptfbbb, ptfbbf) #> new File(outfile)).!
  }

}
class Alt1 extends DatalogAnalysis {
  override def exhaustivePath(): String = datalogDir + "exhaustive1.dl"
  override def demandedPath(): String = datalogDir + "demand1.dl"

  override def demandFiles(): List[String] = List("magic_pointsTo_bf.csv", "magic_pointsToField_bbf.csv")
  override def trackFiles(): List[String] = List("magic_pointsTo_bb.csv", "magic_pointsToField_bbb.csv", "magic_pointsTo_fb.csv")

  override def outputSolution(outfile: String): Unit = {
    val ptfb = datalogDir + "pointsTo_fb.csv"

    ("sort -u %s %s %s %s %s".format(ptbb, ptbf, ptfbbb, ptfbbf, ptfb) #> new File(outfile)).!
  }
}