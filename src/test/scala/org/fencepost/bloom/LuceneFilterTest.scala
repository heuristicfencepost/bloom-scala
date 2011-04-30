package org.fencepost.bloom

import org.scalatest.Suite

import org.apache.commons.lang.RandomStringUtils

class LuceneFilterTest extends Suite {

  def testBasic() = {

    val membercount = 10000

    // Generate set of candidates
    val somestrings = for { i <- 1 to membercount } yield RandomStringUtils.randomAscii(24)
    expect(membercount) { somestrings.size }
    expect(membercount) { somestrings.toStream.size }
    val lucene = new LuceneFilter(somestrings.toStream)

    // All candidates should be present
    val somematches = somestrings filter { arg => lucene contains arg }
    expect(membercount) { somematches.size }

    // Generate a set of new random strings
    val someotherstrings = for { i <- 1 to membercount } yield RandomStringUtils.randomAscii(24)
    val someothermatches = someotherstrings filter { arg => lucene contains arg }

    val falseposrate = someothermatches.size/membercount
    println("False positive size: " + someothermatches.size + " (" + falseposrate + "%)")
  }
}
