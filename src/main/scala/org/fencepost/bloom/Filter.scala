package org.fencepost.bloom

abstract class Filter[T] {

  def contains(arg:T):Boolean
}
