package org.fencepost.bloom

import scala.collection.BitSet
import scala.math.abs

import org.apache.commons.lang.math.RandomUtils

import com.facebook.infrastructure.utils.MurmurHash

class BloomFilter(vals:Seq[String],fncount:Int) extends StringFilter {

  private val hash = new MurmurHash()
  private val maxval = 65536 // 65536 / 8 bits/byte = 8192 bytes = 8k for storage

  // Build a consistent set of hash functions to apply to every input val
  private val fns = (1 to fncount) map { arg =>

    val seed:Int = RandomUtils.nextInt()
    (subarg:String) => abs(hash.hash(subarg.getBytes,subarg.getBytes.length,seed)) % maxval
  }

  // Evaluate our set of hash functions against the input string and return
  // a list containing the results
  private def evalAllFns(arg:String) = fns map { _ apply arg }

  // The heart of the Bloom filter; a list consisting of ints generated by the
  // set of functions for each input.  This probably should be implemented as
  // a bit set rather than a list.
  private val bloom = (BitSet() /: (vals map evalAllFns))(_ ++ _)

  override def contains(arg:String) = {
    val vals = evalAllFns(arg)
    vals forall (bloom contains _)
  }

  override def toString = "Bloom filter: " + bloom
}
