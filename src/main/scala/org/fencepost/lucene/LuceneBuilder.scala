package org.fencepost.lucene

import scala.io.Source
import scala.xml.pull._

object LuceneBuilder {
  
  def parseScript(path:String):Iterator[String] = {
    
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

  def main(args:Array[String]) {

    val pruneRegex = """([a-zA-Z0-9]\S+[a-zA-Z0-9])""".r
    for {
      line <- parseScript(args(0))
      word <- line.split("\\s")
      prunedword <- pruneRegex findFirstIn word
    } println(prunedword)
  }
}
