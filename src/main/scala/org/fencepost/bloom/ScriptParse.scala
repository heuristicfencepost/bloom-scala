package org.fencepost.bloom

import scala.io.Source
import scala.xml.pull._

// Object which implements logic for parsing a given Shakespearean play in
// XML format.
object ScriptParse {

  // We've seen a LINE element so handle it
  def handleLine(reader:XMLEventReader):Stream[String] = {

    reader.next match {

      // straight text gets added to our stream.  We aren't done with the line,
      // however, so we append the discovered text to a recursive call to
      // ourselves.  An end event for the LINE element will return us to
      // getLines()
      case EvText(txt) => Stream.cons(txt,handleLine(reader))
      case EvElemStart(_,"STAGEDIR",_,_) => handleStagedir(reader)
        
      // Entities can be safely ignored for our purposes
      case EvEntityRef(_) => handleLine(reader)
      case EvElemEnd(_,"LINE") => getLines(reader)
    }
  }

  // We've seen a STAGEDIR so handle it
  def handleStagedir(reader:XMLEventReader):Stream[String] = {

    reader.next match {

      // stage direction may have internal text.  If it does we aren't done
      // handle the element yet so recursively call ourselves.  Otherwise
      // exit out on an end element.
      case EvText(txt) => handleStagedir(reader)
      case EvElemEnd(_,"STAGEDIR") => getLines(reader)
    }
  }

  // While there's still something to read iterate through the event stream
  // until we find another LINE element.
  def getLines(reader:XMLEventReader):Stream[String] = {

      if (!reader.hasNext) { return Stream.Empty }
      reader.next match {
        case EvElemStart(_,"LINE",_,_) => handleLine(reader)
        case _ => getLines(reader)
      }
  }

  def getWords(path:String) = {

    val reader = new XMLEventReader(Source.fromFile(path))
    val pruneRegex = """([a-zA-Z0-9]\S+[a-zA-Z0-9])""".r
    for {
      line <- getLines(reader)
      word <- line.split("\\s")
      prunedword = pruneRegex findFirstIn word
      if prunedword != None
    } yield prunedword.get
  }
}
