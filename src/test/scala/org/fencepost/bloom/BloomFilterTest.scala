package org.fencepost.bloom

import org.scalatest.Suite

import org.apache.commons.lang.RandomStringUtils

class BloomFilterTest extends Suite {

  def testBasic() = {

    val candidateCount = 5000

    // Generate set of candidates
    val candidates = for { i <- 1 to candidateCount } yield RandomStringUtils.randomAscii(24)
    val bloom = new BloomFilter(candidates,1)
    
    // All candidates should be present
    val matches1 = candidates filter { arg => bloom contains arg }
    expect(candidateCount) { matches1.size }

    // Generate a set of new random strings
    val somestrings = for { i <- 1 to candidateCount } yield RandomStringUtils.randomAscii(24)
    val matches2 = somestrings filter { arg => bloom contains arg }

    // We shouldn't see more than a 10% false positive rate
    assert(matches2.size < (0.1 * candidateCount))
  }
}
