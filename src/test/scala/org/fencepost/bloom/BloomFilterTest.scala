package org.fencepost.bloom

import org.scalatest.Suite

import org.apache.commons.lang.RandomStringUtils
import org.apache.commons.lang.math.RandomUtils

import com.facebook.infrastructure.utils.MurmurHash

class BloomFilterTest extends Suite {

  def testBasic() = {

    val hash = new MurmurHash()
    val base = RandomUtils.nextInt(1000)
    val hashFnCount = 1

    // Generate set of candidates and compute hash values for each
    val candidates = for { i <- 1 to 5 } yield RandomStringUtils.randomAscii(24)
    val hashes = candidates map { arg:String => hash.hash(arg.getBytes,arg.length,base) }
    print("Hashes: " + hashes)

    // Initial test implementation... still sorting out some ideas
    def computeBloomValue(arg:String):Int = {
      def blooms(base:Int):Stream[Int] = {

        val rv = hash.hash(arg.getBytes,arg.length,base)
        Stream.cons(rv,blooms(rv))
      }
      (0 /: (blooms(0) take hashFnCount))(_ | _)
    }

    val thefilter = (0 /: (candidates map computeBloomValue))(_|_)
    print("Filter: " + thefilter)

    def executeTest(count:Int,bloom:Int,matchcount:Int):Int = {

      if (count == 0)
        return matchcount
      val testval = RandomStringUtils.randomAscii(24)
      val testbloom = computeBloomValue(testval)
      if ((testbloom & bloom) == testbloom)
        executeTest(count - 1,bloom,matchcount + 1)
      else
        executeTest(count - 1,bloom,matchcount)
    }

    val thecount = executeTest(20,thefilter,0)
    println("Count: " + thecount)
  }
}
