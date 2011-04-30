package org.fencepost.bloom

import scala.io._

object FilterApplication {

  def main(args:Array[String]) {

    val scriptwords = ScriptParse getWords args(0)
    val dictwords = Source fromFile args(1)

    val start = System.currentTimeMillis
    val afilter:Filter[String] = args(2) match {
      case "lucene" => new LuceneFilter(dictwords.getLines.toStream).asInstanceOf[Filter[String]]
      case "bloom" => new BloomFilter(dictwords.getLines.toStream,20,1)
    }
    val filtercomplete = System.currentTimeMillis
    println("Filter constructed in " + (filtercomplete - start) + " milliseconds")

    val somematches = scriptwords filter (afilter contains _)
    val searchcomplete = System.currentTimeMillis
    println("Total words in script: " + scriptwords.size)
    println("Matches: " + somematches.size)
    println("Searching completed in " + (searchcomplete - filtercomplete) + " milliseconds")
  }
}
