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

    val button = new Button 
    {
      text = "Pick file to translate"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
      enabled = true
      tooltip = "Click to throw a dart"
    }

    val toggle = new ToggleButton { text = "Toggle" }
    val checkBox = new CheckBox { text = "Check me" }
    
    val textField = new TextField 
    {
      columns = 10
      text = "Click on the target!"
    }

    val inField = new TextField 
    {
      columns = 20
      text = "Click on target!"
    }

    val textArea = new TextArea
    {
      text = "initial text \n line two"
      background = Color.green
    }

    val canvas = new Canvas
    {
      preferredSize = new Dimension( 100, 100 )
    }

    val gridPanel = new GridPanel( 1, 2 )
    {
      contents += checkBox
      contents += label
      contents += textArea
    }

    contents = new BorderPanel
    {
      layout(gridPanel) = North
      layout(button) = West
      layout(canvas) = Center
      layout(toggle) = East
      layout(textField) = South
    }

    size = new Dimension ( 300, 200 )
    menuBar = new MenuBar
    {
      contents += new Menu("File")
      {
        contents += new MenuItem(Action("Exit") { sys.exit(0) })
      }
    }

    listenTo(button)
    listenTo(toggle)
    listenTo(canvas.mouse.clicks)

    reactions += 
    {
      case ButtonClicked(component) if component == button =>
        val glossaryFileName = new File("glossary.txt")
        val chooser = new FileChooser
        val response = chooser.showOpenDialog(null) 
        if (response == FileChooser.Result.Approve) { val glossaryFileName = chooser.selectedFile } 
        textField.text = s"File: $glossaryFileName"
      case ButtonClicked(component) if component == toggle =>
        toggle.text = if (toggle.selected) "On" else "Off"
      case MouseClicked(_, point, _, _, _) =>
        canvas.throwDart(new Dart(point.x, point.y, Color.black))
        textField.text = (s"You clicked in the Canvas at x=${point.x}, y=${point.y}.") 
    }
  }
}

object Translate
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

case class Dart(val x: Int, val y: Int, val color: java.awt.Color)

class Canvas extends Panel 
{
  import scala.swing.Panel
  import java.awt.{ Graphics2D, Color }

  var centerColor = Color.yellow
  
  var darts = List[Dart]()

  override def paintComponent(g: Graphics2D) {
    
    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)
    
    // Draw background here
    g.setColor(Color.blue)
    g.fillOval(0, 0, 100, 100)
    g.setColor(Color.red)
    g.fillOval(20, 20, 60, 60)
    g.setColor(centerColor)
    g.fillOval(40, 40, 20, 20)
    
    // Draw things that change on top of background
    for (dart <- darts) {
      g.setColor(dart.color)
      g.fillOval(dart.x, dart.y, 10, 10)
    }
  }

  /** Add a "dart" to list of things to display */
  def throwDart(dart: Dart) {
    darts = darts :+ dart
    // Tell Scala that the display should be repainted
    repaint()
  }
}