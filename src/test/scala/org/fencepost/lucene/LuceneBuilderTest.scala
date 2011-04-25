package org.fencepost.lucene

import org.scalatest.Suite

import scala.io.Source
import scala.xml.pull._

class LuceneBuilderTest extends Suite {

  def getText(reader:XMLEventReader) = {
    val event = reader.next
    event match {
      case EvText(txt) => txt
    }
  }

  def testReader() = {

    val reader = new XMLEventReader(Source.fromFile("hamlet.xml"))
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
    for (line <- lines take 10) {
      println(line.toString)
    }
  }
}
