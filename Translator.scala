/**
 * Created by Brian Umbarger
 */

import scala.io.Source
import java.lang.String
import scala.collection.mutable.Map

object Translator
{
  def main( args: Array[String] )
  {
    // Please change these values to the appropriate paths/filenames
    val glossaryFileName: String = "glossary.txt"
    val toBeTranslatedFileName: String = "con.txt"
    val translatedTextOutputFileName: String = "translated.txt"

    val glossary = new Glossary(glossaryFileName)
    val writer = new Writer(translatedTextOutputFileName)

    val convert = Source.fromFile(toBeTranslatedFileName).getLines().toList map{_.split(' ').toList }

    for ( eachLine <- convert ) 
    { 
      var wordList = List[String]()
      var defList = List[String]()

      for ( eachWord <- eachLine ) 
      {
        var word: String = eachWord
        var defWord = glossary.translate(eachWord)
        while( word.length > defWord.length ) defWord += " "
        while( word.length < defWord.length ) word += " "
        wordList = word :: wordList
        defList = defWord :: defList
      }
      writer.write(wordList)
      writer.write(defList)
    }
  }

  println("Translated file saved as translated.txt" )
}

class Writer( outFile: String )
{
  import java.io._

  def write( wordList: List[String] ) : Unit = 
  {
    var wordString: String = ""
    
    for( word <- wordList ) 
    {
      wordString +=  word 
      wordString += " " 
    }

    val bw = new BufferedWriter( new FileWriter( new File(outFile), true ))
    bw.write(wordString) 
    bw.newLine() 
    bw.close()
  }
}

class Glossary( glossaryFileName: String )
{
  val glossaryList = Source.fromFile(glossaryFileName).getLines().toList map{ _.split(',').toList }
  val glossaryMap:Map[String, List[String]] = Map()
  val chosenMap:Map[String,Int] = Map()

  for ( glossaryEntry <- glossaryList ) glossaryMap += ( glossaryEntry(0) -> glossaryEntry.drop(1) )

  // printKeys() and printVals() used for debugging
  def printKeys() : Unit = println( " Keys: " + glossaryMap.keys ) 
  def printVals() : Unit = println( "Values: " + glossaryMap.values )

  def translate( word: String ) : String = 
  {
    if( glossaryMap.contains( word ) ) 
    {
      var choice: Int = 0
      val defs: List[String] = glossaryMap( word )
      if( defs.length > 1 )
      {
        if( chosenMap.contains( word )) choice = chosenMap(word)
        else
        {
          var i: Int = 0 
          var choiceString = ""
          for( defn <- defs )
          {
            i += 1
            choiceString += " ["
            choiceString += i 
            choiceString += "] "
            choiceString += defn
          }
          do
          {
            println("More than one definition exists for " + word + ". Please enter translation choice" + choiceString + "." )
            choice = Console.readInt
          } while ( choice < 1 || choice > defs.length )
          choice -= 1
          chosenMap += ( word -> choice )
        }
      }
      return defs(choice)
    }
    else 
    {
      var dashes: String = ""
      for( a <- 1 to word.length() ) dashes += "-" 
      return dashes
    }
  }
}