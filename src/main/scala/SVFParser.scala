import sun.misc.ObjectInputFilter
import ujson.{Value, read}

import java.io.FileReader
import scala.collection.mutable
import scala.sys.process.*


/** Parsing consists of two phases - since the json dump produced by SVF does not correctly display
 * the GepObjVarMap (since the dump is generated before the map is populated), we */

class SVFParser {

  /** Compiles and outputs a C program into LLVM bytecode, invokes SVF to generate constraints and dumps
   * a .json file and .txt containing information about the program. After this, the dumped .json file and .txt file are
   * parsed and a C-style program is generated and returned. All generated files are written to outDir. */
  def programFromJSON(inputFile: String, outDir: String): SVFResult = {
    val cmd = Seq(FileManager.SVF_SCRIPT, inputFile, outDir)
    val exitCode = Process(cmd).!


    if exitCode == 0 then
      val (program, dummyNodes) = parseJsonDump(outDir + FileManager.JSON_DUMP)
      val gepVarObjMap = SVFMapping(outDir + FileManager.GEP_FILE)
      val nodes = parseNodes(outDir+FileManager.CG_FILE)
      
      // Also add all gep nodes
      gepVarObjMap.gepVarObjMap.values.foreach(nodes.add)
      
      println("Nodes are")
      println(nodes)
      
      SVFResult(program, gepVarObjMap, dummyNodes, outDir)
    else
      throw Error(s"Invocation exited with error code $exitCode")
  }

  private def parseNodes(file: String): mutable.Set[Int] = {
    // Nodes to use for points-to computations - ALSO INCLUDE GEP NODES
    val nodes = mutable.Set[Int]()
    val nodePattern = """Node0x([0-9a-f]+) \[.*,label="\{([0-9]+):?(\w+)?}"];""".r

    val content = FileManager.readFile(file)
    val lines = content.trim.split("\n")

    for (line <- lines) {
      line match
        case nodePattern(id, label, name) =>
          nodes.add(label.toInt)
        case _ =>
    }

    nodes
    
  }

  // TODO: I think this might actually be the "solved" constraint graph meaning it would require potentially two
  //  solution passes - one for SVF and another for this tool...
  def programFromPrint(inputFile: String, outDir: String): SVFResult = {
    val cmd = Seq(FileManager.SVF_PRINT_SCRIPT, inputFile, outDir)
    val exitCode = Process(cmd).!

    if exitCode == 0 then
      val dummyNodes = mutable.ArrayBuffer[Cell]()
      val program = parsePrintedCG(outDir + FileManager.PRINT_FILE)
      val gepVarObjMap = SVFMapping(outDir + FileManager.GEP_FILE)
      SVFResult(program, gepVarObjMap, dummyNodes, outDir)
    else
      throw Error(s"Invocation exited with error code $exitCode")
  }

  def programFromConstraintGraph(inputFile: String, outDir: String): SVFResult = {
    val cmd = Seq(FileManager.SVF_DUMP_CG_SCRIPT, inputFile, outDir)
    val exitCode = Process(cmd).!

    if exitCode == 0 then
      val dummyNodes = mutable.ArrayBuffer[Cell]()
      val program = parseConstraintGraphFile(outDir + FileManager.CG_FILE)
      val gepVarObjMap = SVFMapping(outDir + FileManager.GEP_FILE)
      SVFResult(program, gepVarObjMap, dummyNodes, outDir)
    else
      throw Error(s"Invocation exited with error code $exitCode")
  }

  private def parseConstraintGraphFile(str: String): CProgram = {
    val nodePattern = """Node0x([0-9a-f]+) \[.*,label="\{([0-9]+):?(\w+)?}"];""".r
    val edgePattern = """Node0x([0-9a-f]+) -> Node0x([0-9a-f]+)\[color=(\w+)];""".r

    val instructions = mutable.ArrayBuffer[CInstruction]()

    val content = FileManager.readFile(str)
    val lines = content.trim.split("\n")


    val node2id = mutable.Map[String, Int]()

    def generateInstruction(fromId: Int, toId: Int, edgeType: String): CInstruction = {
      edgeType match
        case "green" => AddrOf(toId, fromId)
        case "black" => Copy(toId, fromId)
        case "red" => CLoad(toId, fromId)
    }


    // TODO: Sort to always process node declarations before edges - for now just two-pass

    for (line <- lines) {
      line match
        case nodePattern(id, label, name) =>
          println(s"NodePattern($id, $label, $name")
          // only if non null?
          node2id += id -> label.toInt
        case _ =>
    }


    for (line <- lines) {
      line match
        case edgePattern(idLeft, idRight, color) =>
          println(s"In edgePattern ${idLeft}, ${idRight}, ${color}")
          // Safe to ignore if not found? (I think so since SVF only reports pts for such nodes, but not sure
          if (node2id.contains(idLeft) && node2id.contains(idRight)) {
            instructions.addOne(generateInstruction(node2id(idLeft), node2id(idRight), color))
          }
        case _ =>


    }




    CProgram(instructions.toList)

  }

  private def parsePrintedCG(file: String): CProgram = {
    val instructions = mutable.ArrayBuffer[CInstruction]()

    def parseAndAddInstruction(line: String): Unit = {
      val addrRegExp = """(\d+) -- Addr --> (\d+)""".r
      val copyRegExp = """(\d+) -- Copy --> (\d+)""".r
      val loadRegExp = """(\d+) -- Load --> (\d+)""".r
      val storeRegExp = """(\d+) -- Store --> (\d+)""".r
      val normalGepRegExp = """(\d+) -- NormalGep \((\d+)\) --> (\d+)""".r

      line match
        case addrRegExp(from, to) => instructions.addOne(AddrOf(to.toInt, from.toInt))
        case copyRegExp(from, to) => instructions.addOne(Copy(to.toInt, from.toInt))
        case loadRegExp(from, to) => instructions.addOne(CLoad(to.toInt, from.toInt))
        case storeRegExp(from, to) => instructions.addOne(CStore(to.toInt, from.toInt))
        case normalGepRegExp(from, offset, to) => instructions.addOne(Gep(to.toInt, from.toInt, offset.toInt))
    }

    val contents = FileManager.readFile(file)

    val cgStartDelim = "-----------------ConstraintGraph--------------------------------------"
    val cgEndDelim = "--------------------------------------------------------------"

    val lines = contents.split("\n")
    val zipWithIndex = lines.zipWithIndex

    var cgStartIdx = -1
    var cgEndIdx = -1


    for ((content, idx) <- zipWithIndex) {
      if content == cgStartDelim then
        cgStartIdx = idx + 1
      if content == cgEndDelim then
        cgEndIdx = idx
    }

    val cgLines = lines.slice(cgStartIdx, cgEndIdx)

    for (cgLine <- cgLines) {
      parseAndAddInstruction(cgLine)
    }

    CProgram(instructions.toList)
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
          // Green: AddrOf
          instructions.addOne(AddrOf(dst, src))
        case 1 =>
          // Black: Copy
          instructions.addOne(Copy(dst, src))
        case 2 =>
          // Blue: Store
          instructions.addOne(CStore(dst, src))
        case 3 =>
          // Red: Load
          instructions.addOne(CLoad(dst, src))
        case 4 =>
          // Dashed black: Parameter passing
          instructions.addOne(Copy(dst, src))
        case 5 =>
          // Dotted black: Return
          instructions.addOne(Copy(dst, src))
        case 6 =>
          // Purple: Gep
          // Incoming gep edges holds the base indexing into. The outgoing edge holds the address computed by gep
          val fieldIdx = edge("ap")("fldIdx").str.toInt
          instructions.addOne(Gep(dst, src, fieldIdx))
        case 7 =>
        // Grey: BinOp
        case x => throw new Error(s"Unsupported edge type $x from $src -> $dst -- check dot file to determine color")
    }

    def parseNodeAndMarkIfDummy(node: Value): Unit = {

      // SVF ignores dummy values (i.e., a non-LLVM related values) so mark these based on nodeKind
      // TODO: Any others?
      val DUMMY_KINDS = List(7, 8)

      val id = node("id").num.toInt
      val kind = node("nodeKind").str.toInt


      // TODO: Rework this: If ander.txt has the node specified either in the node section or the GEP section, then
      // we must compute points-to sets for the node. Otherwise it should remain empty.
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

class SVFResult(val program: CProgram, val mapping: SVFMapping, val dummyNodeIds: mutable.ArrayBuffer[Cell], outDir: String) {

  def compareWithSVF(sol: CSolution): Unit = {

    // TODO: Method for extracting delimiter indices
    val anderContent = FileManager.readFile(outDir + FileManager.GEP_FILE)
    // Delimiter used by SVF for ander.txt format
    val delimiter = "------"
    val lines = anderContent.split("\n").toList

    // Find the indices of the delimiters
    val delimiterIndices = lines.zipWithIndex.collect {
      case (content, index) if content == delimiter => index
    }

    val pointsToLines = lines.slice(delimiterIndices.head + 1, delimiterIndices(1))
    val anderSol = mutable.Map[Cell, mutable.Set[Cell]]().withDefaultValue(mutable.Set.empty)

    pointsToLines.foreach(line => {
      val content = line.split("->")
      val key = content(0).replace(" ", "").toInt
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
      println("Provided solution: \n" + sol)
      println()
      println("SVF solution: \n" + anderSol)
      assert(false)


  }
}


class SVFMapping(mappingFile: String) {

  val (gepVarObjMap: mutable.Map[(Var, Int), Var], nodes: mutable.Set[Cell]) = parseMappingFile()

  private def parseMappingFile(): (mutable.Map[(Var, Int), Var], mutable.Set[Cell]) = {
    val map = mutable.Map[(Var, Int), Var]()
    val nodes = mutable.Set[Cell]()
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

    // format: nodeid isFieldSensitive
    val objVarLines = lines.slice(0, delimiterIndices.head)
    objVarLines.foreach(l => nodes.add(l.split(" ")(0).toInt))


    // The GepVarObjMap is stored between delimiter 1 and 2 (using 0 indexing)
    // Extract lines between delimiter 1 and 2
    val gepLines = lines.slice(delimiterIndices(1) + 1, delimiterIndices(2))

    // Format is: baseId offset gepNode
    gepLines.foreach(line => {
      val entry = line.split(" ")
      val baseNode = entry(0).toInt
      val offset = entry(1).toInt
      val gepNode = entry(2).toInt
      map += (baseNode, offset) -> gepNode

      // Also add base node (if not always present?) and gep node to nodes
      nodes.add(baseNode)
      nodes.add(gepNode)

    })
    (map, nodes)
  }


  override def toString: String = this.gepVarObjMap.toString()

}
