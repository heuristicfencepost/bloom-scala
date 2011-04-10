package org.fencepost.bloom

import com.facebook.infrastructure.utils.MurmurHash

class BloomFilter(vals:Seq[String],hashcount:Int) {

  private val hash = new MurmurHash()

  // The Bloom filter bits; apply getBloomBits to all values specified at
  // construction time and combine the results together with a bitwise OR.
  private val bloom = (0 /: (vals map getBloomBits))(_|_)

  // Get the bits to set in our filter for the input string.  This consists of
  // little more than applying the requisite number of hash functions and
  // combining the output via bitwise OR.
  private def getBloomBits(arg:String) = (0 /: (blooms(arg,0) take hashcount))(_ | _)

  // Evaluate the input string against a sequence of hash functions.  Each hash
  // function is built by using the output of the previous hash as the seed
  // value for the subsequent hash function (based on
  // http://spyced.blogspot.com/2009/01/all-you-ever-wanted-to-know-about.html
  private def blooms(arg:String,base:Int):Stream[Int] = {

    val rv = hash.hash(arg.getBytes,arg.length,base)
    Stream.cons(rv,blooms(arg,rv))
  }

  def contains(arg:String) = {
    val bits = getBloomBits(arg)
    (bits & bloom) == bits
  }

  override def toString = "Bloom filter: " + bloom
}
