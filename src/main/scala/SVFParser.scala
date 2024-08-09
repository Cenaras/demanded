import ujson.Value

import java.io.FileReader
import scala.collection.mutable
import scala.sys.process.*


/** Parsing consists of two phases - since the json dump produced by SVF does not correctly display
 * the GepObjVarMap (since the dump is generated before the map is populated), we */

class SVFParser {

  /** Compiles and outputs a C program into LLVM bytecode, invokes SVF to generate constraints and dumps
   * a .json file and .txt containing information about the program. After this, the dumped .json file and .txt file are
   * parsed and a C-style program is generated and returned. All generated files are written to outDir. */
  def programFromCFile(inputFile: String, outDir: String): SVFResult = {
    val cmd = Seq(FileManager.SVF_SCRIPT, inputFile, outDir)
    val exitCode = Process(cmd).!


    if exitCode == 0 then
      val (program, dummyNodes) = parseJsonDump(outDir+FileManager.JSON_DUMP)
      val gepVarObjMap = GepVarObjMap(outDir+FileManager.GEP_FILE)
      SVFResult(program, gepVarObjMap, dummyNodes, outDir)
    else
      throw Error(s"Invocation exited with error code $exitCode")
  }


  /** Parses a json dump from SVF (generated via -dump-json) and generates a corresponding C-style program. */
  private def parseJsonDump(path: String): (CProgram, mutable.ArrayBuffer[Cell]) = {
    val instructions = mutable.ArrayBuffer[CInstruction]()
    val dummyNodes = mutable.ArrayBuffer[Cell]()

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

    def parseNodeAndMarkIfDummy(node: Value): Unit = {

      // SVF ignores dummy values (i.e., a non-LLVM related values) so mark these based on nodeKind
      // TODO: Any others?
      val DUMMY_KINDS = List(7, 8)

      val id = node("id").num.toInt
      val kind = node("nodeKind").str.toInt
      if (DUMMY_KINDS.contains(kind)) then
        dummyNodes.addOne(id)
    }

    val jsonString = FileManager.readFile(path)
    val parsed = ujson.read(jsonString)
    val irGraph = parsed("irGraph")

    val nodes = irGraph("allNode").arr
    for (node <- nodes) {
      parseNodeAndMarkIfDummy(node)
    }


    val edges = irGraph("allEdge").arr
    for (edge <- edges) {
      parseEdgeAndAddInstruction(edge)
    }

    (CProgram(instructions.toList), dummyNodes)
  }
}

class SVFResult(val program: CProgram, val gepVarObjMap: GepVarObjMap, val dummyNodeIds: mutable.ArrayBuffer[Cell], outDir: String) {

  def compareWithSVF(sol: CSolution): Unit = {

    // TODO: Method for extracting delimiter indices
    val anderContent = FileManager.readFile(outDir+FileManager.GEP_FILE)
    // Delimiter used by SVF for ander.txt format
    val delimiter = "------"
    val lines = anderContent.split("\n").toList

    // Find the indices of the delimiters
    val delimiterIndices = lines.zipWithIndex.collect {
      case (content, index) if content == delimiter => index
    }

    val pointsToLines = lines.slice(delimiterIndices.head +1, delimiterIndices(1))
    val anderSol = mutable.Map[Cell, mutable.Set[Cell]]().withDefaultValue(mutable.Set.empty)

    pointsToLines.foreach(line => {
      val content = line.replace(" ", "").split("->")
      val key = content(0).toInt
      val pointsToString = content(1)

      val numRegExp = "\\d+".r
      val pointsToSet = numRegExp.findAllIn(pointsToString).map(_.toInt).toList

      for (t <- pointsToSet) {
        if !anderSol.contains(key) then
          val fresh = mutable.Set[Cell]()
          anderSol += key -> fresh
        anderSol(key).add(t)
      }
    })

    println("Comparing provided solution with SVF produced solution for the input program...")
    if sol == anderSol then
      println("Solutions were identical!")
    else
      println("Solution mismatch!")
      println("Provided solution: \n"+sol)
      println()
      println("SVF solution: \n"+anderSol)
      assert(false)


  }
}


class GepVarObjMap(mappingFile: String) {

  val mapping: mutable.Map[(Var, Int), Var] = parseMappingFile()


  def get(base: Var, offset: Int): Var = mapping(base, offset)

  private def parseMappingFile(): mutable.Map[(Var, Int), Var] = {
    val map = mutable.Map[(Var, Int), Var]()
    val mapFileContent = FileManager.readFile(mappingFile)

    // Delimiter used by SVF for ander.txt format
    val delimiter = "------"

    val lines = mapFileContent.split("\n").toList

    // Find the indices of the delimiters
    val delimiterIndices = lines.zipWithIndex.collect {
      case (content, index) if content == delimiter => index
    }

    // assert format by ensuring 3 delimiters were found
    assert(delimiterIndices.length == 3)

    // The GepVarObjMap is stored between delimiter 1 and 2 (using 0 indexing)
    // Extract lines between delimiter 1 and 2
    val gepLines = lines.slice(delimiterIndices(1)+1, delimiterIndices(2))

    // Format is: baseId offset gepNode
    gepLines.foreach(line => {
      val entry = line.split(" ")
      map += (entry(0).toInt, entry(1).toInt) -> entry(2).toInt
    })
    map
  }


  override def toString: String = this.mapping.toString()

}
