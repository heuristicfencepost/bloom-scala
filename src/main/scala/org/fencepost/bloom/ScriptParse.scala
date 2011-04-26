package org.fencepost.bloom

import scala.io.Source
import scala.xml.pull._

// Object which implements logic for parsing a given Shakespearean play in
// XML format.
object ScriptParse {
  
  def getLines(path:String) = {
    
    def getText(reader:XMLEventReader):String = {
      val event = reader.next
      event match {
        case EvText(txt) => txt
        case EvElemStart(_,_,_,_) => {
            reader.next // sub-element text event
            reader.next // sub-element end event
            getText(reader)
        }
        case EvElemEnd(_,_) => "" // can happen if STAGEDIR is only value for line
      }
    }
    
    val reader = new XMLEventReader(Source.fromFile(path))
    val urlines = reader map { event:XMLEvent =>
      event match {
        case EvElemStart(_,"LINE",_,_) => Some(getText(reader))
        case _ => None
      }
    }
    val lines = urlines filter { aval:Option[String] =>
      aval match {
        case Some(astr) => true
        case None => false
      }
    }
    lines map (_.get)
  }

  def getWords(path:String) = {

    val pruneRegex = """([a-zA-Z0-9]\S+[a-zA-Z0-9])""".r
    for {
      line <- getLines(path)
      word <- line.split("\\s").toIterator
      prunedword = pruneRegex findFirstIn word
      if prunedword != None
    } yield prunedword.get
  }
}
