package org.fencepost.bloom

import org.scalatest.Suite

import org.apache.commons.lang.RandomStringUtils
import org.apache.commons.lang.math.RandomUtils

class BloomFilterTest extends Suite {

  def testBasic() = {

    val candidateCount = 2

    // Generate set of candidates
    val candidates = for { i <- 1 to candidateCount } yield RandomStringUtils.randomAscii(24)
    val bloom = new BloomFilter(candidates,1)
    println(bloom toString)
    
    // All candidates should be present
    val matches1 = candidates filter { arg => bloom contains arg }
    expect(candidateCount) { matches1.size }

    // Generate a set of new random strings
    val somestrings = for { i <- 1 to 20 } yield RandomStringUtils.randomAscii(24)
    val matches2 = somestrings filter { arg => bloom contains arg }
    expect(0) { matches2.size }
  }
}
