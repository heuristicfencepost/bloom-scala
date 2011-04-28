package org.fencepost.bloom

import org.scalatest.Suite

import org.apache.commons.lang.RandomStringUtils

class BloomFilterTest extends Suite {

  def testBasic() = {

    val membercount = 10000

    // Generate set of candidates
    val somestrings = for { i <- 1 to membercount } yield RandomStringUtils.randomAscii(24)
    expect(membercount) { somestrings.size }
    expect(membercount) { somestrings.toStream.size }
    val bloom = new BloomFilter(somestrings.toStream,18,2)
    
    // All candidates should be present
    val somematches = somestrings filter { arg => bloom contains arg }
    expect(membercount) { somematches.size }

    // Generate a set of new random strings
    val someotherstrings = for { i <- 1 to membercount } yield RandomStringUtils.randomAscii(24)
    val someothermatches = someotherstrings filter { arg => bloom contains arg }

    // We shouldn't see more than a 10% false positive rate
    println("False positive size: " + someothermatches.size)
    //assert(matches2.size < (0.2 * candidateCount))
  }
}
