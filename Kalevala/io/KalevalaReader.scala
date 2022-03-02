package io

import java.io.FileReader
import java.io.FileNotFoundException
import java.io.BufferedReader
import java.io.IOException
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.BufferedReader

/**
 * In this exercise you will practice the basics of file handling. You will be
 * handling a part of a Finnish poem from the Finnish folk lore epic Kalevala.
 *
 * We will be using the BufferedReader and the FileReader classes from the
 * java.io package to read the poem line by line. When reading a file this way, it
 * is possible to choose some parts of the file and leave some parts out. In the exercise
 * you will be making different filters to choose only particular lines of the poem in each part.
 *
 * In the exercise there is included the text file of the third poem of the Kalevala epic (kalevala3.txt).
 * The poem consists of lines of the poem separated into verses with an empty line. Thus, from the empty
 * lines you will know that a new verse will start on the following line.
 *
 * In the exercise we want to design filters that will extract only certain lines from the poem and save
 * them into a string sequence Seq[String]. The filter methods takes the BufferedWriter as a parameter which
 * can be used to read through a file line by line.
 *
 * You can take a look at the noFilter method to get an idea how the filter methods are supposed to traverse
 * through the lines of the file.
 *
 * Finally once your filters are working correctly the writeToFile method should write the results of your
 * file reading and filtering to a new file. Here we use FileWriter and BufferedWriter.
 *
 * The writeToFile takes a string sequence as a parameter, which corresponds to the sequence of the filtered
 * lines produced by the filters. This method should simply write the contents of the string sequence to a
 * designated file location.
 *
 * Note: Included in the project, in the "models" folder, are example results of what your filters should produce.
 * You can use these files to compare your output and what the result should look like to help you
 * come up what you might need to change in your solution if the results don't match. Don't make changes
 * to these files as they are used to determine your local unit tests.
 */

object KalevalaReader {
  def alphabet = "abcdefghijklmnopqrstuvxyzåäöABCDEFGHIJKLMNOPQRSTUVXYZÅÄÖ\""
  private val poemFile = "kalevala3.txt"

  private val targets =
    Array("no_filter.txt",
        "first_line.txt",
        "vainamoinen.txt",
        "every_other.txt") // File names for the different filters.

  private val filters =
    Seq[(BufferedReader => Seq[String])](
        noFilter,
        firstLineOfEachVerse,
        rowsWithVainamoinen,
        everyOtherRowOfVerses)  //All the different filters.

  /**
   * Helper method for reading the file with all the different filters and writing the results into a file.
   */
  def writeAllFilters(source: String, filters: Seq[BufferedReader => Seq[String]], targets: Seq[String])
    = (filters zip targets).foreach{case(f, t) => writeToFile(t, readFile(source, f))}

  /**
   * Write a method that reads a file using a FileReader to read the file
   * and a BufferedReader to read the lines in the file. Use the
   * BufferedReader as a parameter for your FileOperation function.
   *
   * Check for Exceptions in the code. Particularly whether the file was found
   * and whether there was a reading error i.e. IOException.
   *
   * The reader should return a sequence containing each line of the filtered file.
   */
  def readFile(sourceFile: String, fileOperation: BufferedReader => Seq[String]): Seq[String] = {
    val fileReader = try {
      new FileReader(sourceFile)
    } catch {
      case e: FileNotFoundException => return Seq[String]()
    }

    val lineReader = new BufferedReader(fileReader)

    try {
      fileOperation(lineReader)
    } catch {
      case e: IOException => return Seq[String]()
    }
  }
Some(Double.PositiveInfinity)

  /**
   * A helper method that shows you how to read a file without applying any
   * filtering.
   */
  def noFilter(lineReader: BufferedReader) = {
    var resList = Seq[String]()
    var oneLine: String = null
    while ({oneLine = lineReader.readLine(); oneLine != null}) {
      resList = resList :+ oneLine
    }
    resList
  }

  /**
   * Returns the first line of each verse. Here you need to
   * be able to distinguish the separation of two consecutive verses as well
   * as pick only the first line of each of the verses you find.
   */

  def firstLineOfEachVerse(lineReader: BufferedReader): Seq[String] = {
    var text = this.noFilter(lineReader).drop(1)
    var line = Seq[String]()
    while (text.nonEmpty) {
      val verse = text.takeWhile( _ != "" ) :+ ""
      if(verse.head != "") line = line :+ verse.head
      text = text.drop(verse.size)
    }
    return line
  }



  /**
   * Returns a sequence of the rows that contain
   * the string "Väinämöinen". Make sure that you have the
   * initial letter capitalized.
   */
  def rowsWithVainamoinen(lineReader: BufferedReader): Seq[String] = {
    var v_line = Seq[String]()
    var line: String = lineReader.readLine()
    while (line != null) {
      if(line.contains("Väinämöinen")) v_line = v_line :+ line
      line = lineReader.readLine()
    }
    return v_line
  }


  /**
   * Reads the file and returns a version where every other line in each verse
   * starting from the second line is filtered out. E.g. you keep the lines
   * 1, 3, 5, ... This method keeps the header line.
   *
   * Note: You should keep the line spaces between verses!
   */
  def everyOtherRowOfVerses(lineReader: BufferedReader): Seq[String] = {
    var text = this.noFilter(lineReader)
    var other_line = Seq[String]()
    while (text.nonEmpty) {
      val verse = text.takeWhile( _ != "" ) :+ ""
      var counter = 0
      for (n <- verse) {
        if(counter % 2 == 0 || n == "") other_line = other_line :+ n
        counter += 1
      }
      text = text.drop(verse.size)
    }
    other_line.dropRight(1)
  }


  /**
   * Create a method for writing the contents of a string sequence into a file.
   *
   * Here you don't have to consider any filters, but simply write each
   * string in the string sequence onto its own line.
   *
   * You can use the FileWriter to access the file and the BufferedWriter to
   * write strings onto separate lines.
   *
   * Again consider exceptions, e.g. whether the file was found
   * and whether there was a reading error i.e. IOException.
   **/

  def writeToFile(fileName: String, arr: Seq[String]) = {
    try {
      val file = new FileWriter(fileName)
      val out_line = new BufferedWriter(file)
      try {
        arr.foreach (line => out_line.write(line) )
      } finally {
        file.close()
        out_line.close()
      }
    } catch {
      case e1: FileNotFoundException => println("File not found.")
      case e2: IOException           => println("IOException: Write finished with errors")
    }
  }


}
