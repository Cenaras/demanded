import ujson.Value
import scala.collection.mutable
import scala.sys.process._

class SVFParser {

  /** Compiles and outputs a C program into LLVM bytecode, invokes SVF to generate constraints and dumps
   * a .json file containing information about the program. After this, the dumped .json file is parsed and
   * a C-style program is generated and returned. */
  def programFromCFile(inputFile: String, bcFile: String, jsonDumpFile: String): CProgram = {
    val scriptPath = "./untitled/svf.sh"
    val cmd = Seq(scriptPath, inputFile, bcFile, jsonDumpFile)
    val exitCode = Process(cmd).!

    if exitCode == 0 then
      parseJsonDump(jsonDumpFile)
    else
      throw Error(s"Invocation exited with error code $exitCode")
  }


  /** Parses a json dump from SVF (generated via -dump-json) and generates a corresponding C-style program. */
  private def parseJsonDump(path: String): CProgram = {
    val instructions = mutable.ArrayBuffer[CInstruction]()

    /** Extracts information from the json encoded edge and adds the corresponding instruction */
    def parseEdgeAndAddInstruction(edge: Value): Unit = {
      // The 8 least significant bits describe the edge flag so use 0xFF as mask to extract those
      val FLAG_MASK = 0xFF
      val edgeType = edge("edgeFlag").str.toLong & FLAG_MASK

      val src: Int = edge("src").num.toInt
      val dst = edge("dst").num.toInt
      edgeType match
        case 0 =>
          instructions.addOne(AddrOf(dst, src))
        case 1 =>
          instructions.addOne(Copy(dst, src))
        case 2 =>
          instructions.addOne(CStore(dst, src))
        case 3 =>
          instructions.addOne(CLoad(dst, src))
        case 6 =>
          // Incoming gep edges holds the base indexing into. The outgoing edge holds the address computed by gep
          val fieldIdx = edge("ap")("fldIdx").str.toInt
          instructions.addOne(Gep(dst, src, fieldIdx))
        case 7 =>
        // BinOp edge
        case x => throw new Error(s"Unsupported edge type $x from $src -> $dst -- check dot file to determine color")
    }

    val jsonString = FileManager.readFile(path)
    val parsed = ujson.read(jsonString)
    val edges = parsed("irGraph")("allEdge").arr
    for (edge <- edges) {
      parseEdgeAndAddInstruction(edge)
    }

    CProgram(instructions.toList)
  }
}
