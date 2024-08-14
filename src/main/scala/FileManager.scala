object FileManager {

  /** Reads a file and returns the contents as a newline separated string. Contents from the file is trimmed according
   * to the String#trim method.
   *
   * @param path path of the file to read
   * @return newline separated trimmed string content of file.
   * */
  def readFile(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val lines = try source.getLines().map(_.trim).mkString("\n")
    finally source.close()
    lines
  }

  /** Names of relevant files */
  val C_DIR = "untitled/c-programs/"
  val JSON_DUMP = "/dump.json"
  val GEP_FILE = "/ander.txt"
  val PRINT_FILE = "/output.txt"
  val CONSTRAINT_GRAPH_FILE = "/consCG_initial.dot"
  val PAG_FILE = "/svfir_initial.dot"
  val CALLGRAPH_FILE = "/callgraph_final.dot"

  val SVF_SCRIPT = "./untitled/svf.sh"
  val SVF_PRINT_SCRIPT = "./untitled/svf-print.sh"
  val SVF_DUMP_CG_SCRIPT = "./untitled/svf-dump-cg.sh"



  /** Given a filename of a c program, returns its path in the C_DIR */
  def CPath(filename: String): String = C_DIR + filename
  
  
}
