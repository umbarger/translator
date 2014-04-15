/**
 * Created by Brian Umbarger
 */

import scala.io.Source
import java.lang.String
import scala.collection.mutable.Map
import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import scala.swing.FileChooser
import scala.swing.Component
import java.io.File


object TranslatorApp extends SimpleSwingApplication
{

  def top = new MainFrame {
    title = "Translation App"

    val label = new Label 
    {
      text = "Text File Translator"
      font = new Font( "Ariel", java.awt.Font.ITALIC, 24 )
    }

    val glossaryButton = new Button
    {
      text = "Select Glossary"
      foreground = Color.green
      background = Color.white
      borderPainted = true
      enabled = true
      tooltip = "Select the glossary file."
    }

    val inButton = new Button 
    {
      text = "File to Translate"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
      enabled = false
      tooltip = "Click to select a text file to translate."
    }

    val saveButton = new Button
    {
      text = "Save as:"
      foreground = Color.red
      background = Color.blue
      borderPainted = true
      enabled = false
      tooltip = "Click to select translation name and directory."
    }

    val runButton = new Button
    {
      text = "Translate!"
      foreground = Color.black
      background = Color.green
      borderPainted = true
      enabled = false
      tooltip = "Make sure you pick your input and save as files first."
    }
    
    val textField = new TextField 
    {
      columns = 10
      text = "Please select a file to translate."
    }

   val gridPanel = new GridPanel( 1, 4 )
    {
       contents += glossaryButton
       contents += inButton
       contents += saveButton
       contents += runButton
     }

    contents = new BorderPanel
    {
      layout(label) = North
      layout(gridPanel) = Center
      layout(textField) = South
    }

    size = new Dimension ( 400, 175 )
    menuBar = new MenuBar
    {
      contents += new Menu("File")
      {
        contents += new MenuItem(Action("Exit") { sys.exit(0) })
      }
    }

    listenTo(glossaryButton)
    listenTo(inButton)
    listenTo(saveButton)
    listenTo(runButton)

    var glossaryFileName = new File("glossary.txt")
    var toBeTranslatedFileName = new File("con.txt")
    var translatedTextOutputFileName = new File("translated.txt")

    reactions += 
    {
      case ButtonClicked(component) if component == glossaryButton =>
        //val glossaryFileName = new File("glossary.txt")
        val chooser = new FileChooser
        val response = chooser.showOpenDialog(null) 
        if (response == FileChooser.Result.Approve) { glossaryFileName = chooser.selectedFile } 
        textField.text = s"Glossary: $glossaryFileName - Now pick file to translate."
        glossaryButton.text = "Got it!"
        inButton.enabled = true
      case ButtonClicked(component) if component == inButton =>
        //val toBeTranslatedFileName = new File("con.txt")
        val chooser = new FileChooser
        val response = chooser.showOpenDialog(null) 
        if (response == FileChooser.Result.Approve) { toBeTranslatedFileName = chooser.selectedFile } 
        textField.text = s"File: $toBeTranslatedFileName - Now pick where to save."
        inButton.text = "Got it!"
        saveButton.enabled = true
      case ButtonClicked(component) if component == saveButton =>
        //val translatedTextOutputFileName = new File("translated.txt")
        val chooser = new FileChooser
        val response = chooser.showSaveDialog(null)  
        if (response == FileChooser.Result.Approve) { translatedTextOutputFileName = chooser.selectedFile } 
        textField.text = s"Saving as: $translatedTextOutputFileName - Ready to translate!"
        inButton.text = "Got it!"
        runButton.enabled = true
      case ButtonClicked(component) if component == runButton =>
        val translator = new Translator( glossaryFileName, toBeTranslatedFileName, translatedTextOutputFileName )
    }
  }
}

class Translator( var glossaryFileName: File, var toBeTranslatedFileName: File, var translatedTextOutputFileName: File )
{
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
  println(s"Translated file saved as $translatedTextOutputFileName" )
}


class Writer( outFile: File )
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

    val bw = new BufferedWriter( new FileWriter( outFile, true ))
    bw.write(wordString) 
    bw.newLine() 
    bw.close()
  }
}

class Glossary( glossaryFileName: File )
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