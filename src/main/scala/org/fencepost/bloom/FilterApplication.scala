package org.fencepost.bloom

import scala.io._

object FilterApplication {

  def main(args:Array[String]) {

    val scriptWords = ScriptParse getWords args(0)
    val dictWords = Source fromFile args(1)

    val start = System.currentTimeMillis
    val thefilter:Filter[String] = args(2) match {
      case "lucene" => new LuceneFilter(dictWords.getLines.toStream).asInstanceOf[Filter[String]]
      case "bloom" => new BloomFilter(dictWords.getLines.toStream,20,1)
    }
    val filterComplete = System.currentTimeMillis
    println("Filter constructed in " + (filterComplete - start) + " milliseconds")

    val matches = scriptWords filter (thefilter contains _)
    val searchComplete = System.currentTimeMillis
    println("Total words in script: " + scriptWords.size)
    println("Matches: " + matches.size)
    println("Searching completed in " + (searchComplete - filterComplete) + " milliseconds")
  }
}
