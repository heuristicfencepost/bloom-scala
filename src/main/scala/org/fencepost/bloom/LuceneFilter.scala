package org.fencepost.bloom

// Filter implementation based on Lucene.  Mainly used here for comparison to
// Bloom filter results.
class LuceneFilter(vals:Stream[String]) extends Filter[String] {

  override def contains(arg:String) = {
    false
  }
}
