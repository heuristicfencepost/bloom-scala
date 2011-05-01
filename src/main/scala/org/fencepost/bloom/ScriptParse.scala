package org.fencepost.bloom

import scala.io.Source
import scala.xml.pull._

// Parsing implementation for XML representations of Shakespeare plays provided
// by ibiblio.  This implementation is loosely represented as a finite state
// machine driven by inputs extracted from Scala's built-in event-based XML
// parser.
object ScriptParse {

  // Handle a line to be spoken by an actor
  private def handleLine(reader:XMLEventReader):Stream[String] = {

    reader.next match {

      // Straight text gets added to our stream.  We aren't done with the line,
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

  // Handle a stage direction that might be embedded within a line to
  // be spoken by an actor.
  private def handleStagedir(reader:XMLEventReader):Stream[String] = {

    reader.next match {

      // Stage direction may have internal text.  If it does we aren't done
      // handle the element yet so recursively call ourselves.  Otherwise
      // exit out on an end element.
      case EvText(txt) => handleStagedir(reader)
      case EvElemEnd(_,"STAGEDIR") => getLines(reader)
    }
  }

  // While there's still something to read iterate through the event stream
  // until we find another LINE element.
  private def getLines(reader:XMLEventReader):Stream[String] = {

      if (!reader.hasNext) { return Stream.Empty }
      reader.next match {
        case EvElemStart(_,"LINE",_,_) => handleLine(reader)
        case _ => getLines(reader)
      }
  }

  // The only exposed part of our API; return a stream consisting of all
  // words found within the script represented by the input argument.
  def getWords(path:String) = {

    val reader = new XMLEventReader(Source.fromFile(path))
    val pruneRegex = """([a-zA-Z0-9'\-]*[a-zA-Z0-9])""".r
    for {
      line <- getLines(reader)
      word <- line.split("\\s")
      prunedword = pruneRegex findFirstIn word
      if prunedword != None
    } yield prunedword.get
  }
}
