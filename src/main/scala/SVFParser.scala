import scala.collection.mutable
import scala.util.matching.Regex

class SVFParser {
  val nodePattern: Regex = """Node0x([0-9a-f]+) \[.*,label="\{([0-9]+):?([\w]+)?}"];""".r
  val edgePattern: Regex = """Node0x([0-9a-f]+) -> Node0x([0-9a-f]+)\[color=([\w]+)\];""".r

  val node2id = mutable.Map[String, Int]()


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
