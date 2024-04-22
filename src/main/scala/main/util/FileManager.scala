package main.util

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

}
