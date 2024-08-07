import ujson.Value

import scala.collection.mutable
import scala.util.matching.Regex

class SVFParser {
  val nodePattern: Regex = """Node0x([0-9a-f]+) \[.*,label="\{([0-9]+):?([\w]+)?}"];""".r
  val edgePattern: Regex = """Node0x([0-9a-f]+) -> Node0x([0-9a-f]+)\[color=([\w]+)\];""".r

  val node2id = mutable.Map[String, Int]()


  def parseJsonDump(path: String): CProgram = {
    var instructions = mutable.ArrayBuffer[CInstruction]()

    // TODO: Edge type is encoded as first 8 bits of edgeFlag and remaining is call site location

    def parseEdgeAndAddInstruction(edge: Value): Unit = {
      // Extract required edge information



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








  // TODO: Fields (hardcoded to 0 now)
  private def generateInstruction(fromId: Int, toId: Int, edgeType: String): CInstruction = {
    edgeType match
      case "green" =>
        AddrOf(toId, fromId)
      case "black" =>
        Copy(toId, fromId)
      case "red" =>
        CLoad(toId, fromId)
      case "blue" =>
        CStore(toId, fromId)
      case unknown => throw new Error(s"Unsupported edge color ${unknown}")
  }

  def parseSVF(constraint_file: String): CProgram = {

    val content = FileManager.readFile(constraint_file)
    val lines = content.trim.split("\n")

    // TODO: Always node declarations before edges. For now just do a two-pass

    for (line <- lines) {
      line match
        case nodePattern(id, label, optional) =>
          println(s"In node pattern\n${line}")
          node2id += id -> label.toInt
        case _ =>
      //          println(s"No match for \n${line}")
    }


    val insn = lines.foldLeft(List[CInstruction]())((acc, line) => {
      line match
        case edgePattern(idLeft, idRight, color) => {
          println(s"In edgePattern ${idLeft}, ${idRight}, ${color}")
          generateInstruction(node2id(idLeft), node2id(idRight), color) :: acc
        }
        case _ => acc // TODO: ...
    })

    CProgram(insn)


    //    for (line <- lines) {
    //      line match
    //        case nodePattern(id, label, optional) => println(s"id=${id}, label=${label}, optional=${optional}")
    //        case edgePattern(idLeft, idRight, color) => println(s"${idLeft} --> ${idRight} with color=${color}")
    //        case _ =>
    //    }


    // Use id as the identifier for variables - keep map that maps the hex id back into human readable values
    // Parse the constraint type based on the color of the edge


  }

}
