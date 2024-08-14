import ujson.{Value, read}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.*


// Maps from nodeId, offset to gep node - used to support field sensitivity
type GepVarObjMap = mutable.Map[(Var, Int), Var]


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
      val (program, indCallSiteMap) = parseJsonDump(outDir + FileManager.JSON_DUMP)
      val gepVarObjMap = parseGepVarObjMap(outDir + FileManager.GEP_FILE)

      val nodes = parseNodes(outDir + FileManager.CG_FILE, gepVarObjMap)
      println("Nodes: \n" + nodes)

      // TODO: Need a JSON Result that we can throw everything into. We need the map from callSite to arguments
      //  (callsiteArgList) and the map from function to formal (funArgListMap)
      // 23 --> 32, 34 which are the actual params for indirect call site 23.
      // 1 --> 9, 10 which are the formal parameters - IDK why the key is 1 though...

      // The issue is the call graph stuff - we need a way to resolve it...
      // We can dump the call graph to get call graph node id's

      // We know that node 23 is an indirect function call and targets function pointer 41
      // That means the function we are calling is whatever is in pts(41) - in this case 7 which is the base object for function swap
      // This is correct! - However the mapping information maps not from this, but from some other index, into the parameters.
      // It seems that the function ID that is used is just the reverse declaration order, i.e. bottom-up, maybe we
      // could use that?
      
      
      SVFResult(program, gepVarObjMap, nodes, indCallSiteMap, outDir)
    else
      throw Error(s"Invocation exited with error code $exitCode")
  }

  private def parseNodes(file: String, gepVarObjMap: GepVarObjMap): mutable.Set[Cell] = {
    // Nodes to use for points-to computations - ALSO INCLUDE GEP NODES
    val nodes = mutable.Set[Cell]()
    val nodePattern = """Node0x([0-9a-f]+) \[.*,label="\{([0-9]+):?(.+)?}"];""".r

    val content = FileManager.readFile(file)
    val lines = content.trim.split("\n")

    for (line <- lines) {
      line match
        case nodePattern(id, label, name) =>
          nodes.add(label.toInt)
        case _ =>
    }

    // Also add all gep nodes
    gepVarObjMap.values.foreach(nodes.add)
    nodes
  }

  // TODO: I think this might actually be the "solved" constraint graph meaning it would require potentially two
  //  solution passes - one for SVF and another for this tool...
  def programFromPrint(inputFile: String, outDir: String): SVFResult = {
    val cmd = Seq(FileManager.SVF_PRINT_SCRIPT, inputFile, outDir)
    val exitCode = Process(cmd).!

    if exitCode == 0 then
      val program = parsePrintedCG(outDir + FileManager.PRINT_FILE)
      val gepVarObjMap = parseGepVarObjMap(outDir + FileManager.GEP_FILE)

      val nodes = parseNodes(outDir + FileManager.CG_FILE, gepVarObjMap)

      // TODO...
      SVFResult(program, gepVarObjMap, nodes, mutable.Map[Int, Int](), outDir)
    else
      throw Error(s"Invocation exited with error code $exitCode")
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
  private def parseJsonDump(path: String): (CProgram, mutable.Map[Int, Int]) = {
    val instructions = mutable.ArrayBuffer[CInstruction]()

    // TODO: Enum for colors

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
          // Black: Copy
          instructions.addOne(Copy(dst, src))
        case 9 | 10 | 12 =>
          // Grey: NO-OP (Path/flow sensitive edges)
          println("Are these truly NOOPS?")

        case x => throw new Error(s"Unsupported edge type $x from $src -> $dst -- check dot file to determine color")
    }


    def parseIndirectCallsiteMap(arr: ArrayBuffer[Value]): mutable.Map[Int, Int] = {
      arr.foldLeft(mutable.Map[Int, Int]())((map, value) => {
        map += value.arr(0).num.toInt -> value.arr(1).num.toInt
      })
    }


    val jsonString = FileManager.readFile(path)
    // Parse all edges and add corresponding instructions
    ujson.read(jsonString)("irGraph")("allEdge").arr.foreach(parseEdgeAndAddInstruction)

    // Parse map from array of tuples
    val map = ujson.read(jsonString)("indCallSiteToFunPtrMap").arr
      .foldLeft(
        mutable.Map[Int, Int]())
      ((map, value) =>
        map += value.arr(0).num.toInt -> value.arr(1).num.toInt)

    (CProgram(instructions.toList), map)
  }


  private def parseGepVarObjMap(file: String): GepVarObjMap = {

    val map = mutable.Map[(Var, Int), Var]()
    val mapFileContent = FileManager.readFile(file)

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
    val gepLines = lines.slice(delimiterIndices(1) + 1, delimiterIndices(2))

    // Format is: baseId offset gepNode
    gepLines.foreach(line => {
      val entry = line.split(" ")
      val baseNode = entry(0).toInt
      val offset = entry(1).toInt
      val gepNode = entry(2).toInt
      map += (baseNode, offset) -> gepNode
    })
    map
  }
}

class SVFResult(val program: CProgram, val gepVarObjMap: GepVarObjMap, val nodes: mutable.Set[Cell], indCallsiteMap: mutable.Map[Int, Int], outDir: String) {

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

