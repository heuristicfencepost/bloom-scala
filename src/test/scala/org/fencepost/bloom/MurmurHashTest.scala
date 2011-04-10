package org.fencepost.bloom

import org.scalatest.Suite

import org.apache.commons.lang.RandomStringUtils
import org.apache.commons.lang.math.RandomUtils

import com.facebook.infrastructure.utils.MurmurHash

class MurmurHashTest extends Suite {

  def testDistinct() = {

    val hash = new MurmurHash()
    val base = RandomUtils.nextInt(1000)
    val foo = for { i <- 1 to 100 } yield RandomStringUtils.randomAscii(24)
    val bar = foo map { arg:String => hash.hash(arg.getBytes,arg.length,base) }

    val s = Set() ++ bar
    expect(100) { s.size }
  }
  
  def testRepeatable() = {

    val hash = new MurmurHash()
    val base = RandomUtils.nextInt(1000)
    val teststring = RandomStringUtils.randomAscii(24)
    val comparison = hash.hash(teststring.getBytes,teststring.length,base)
    for (i <- 1 to 100) {
      expect (comparison) { hash.hash(teststring.getBytes,teststring.length,base) }
    }
  }
}
