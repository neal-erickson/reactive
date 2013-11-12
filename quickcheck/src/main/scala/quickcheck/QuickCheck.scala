package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def checkHeapOrder(h: H): Boolean = {
    def checkHeapStep(heap: H, min: Int): Boolean = {
      if(isEmpty(heap)){
        true
      } else {
        val nextMin = findMin(heap)
        //output = output.concat("(" + min + " <= " + nextMin + " " + (min <= nextMin) + ")")
        (min <= nextMin) && checkHeapStep(deleteMin(heap), nextMin)
      }
    }
    checkHeapStep(h, Int.MinValue)
  }
  
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // proves heap1 & heap2 wrong
  property("gen1") = forAll { (h: H) =>
	  val m = if (isEmpty(h)) 0 else findMin(h)
	  findMin(insert(m, h)) == m
	}
  
  property("del1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    isEmpty(h2)
  }
  
  property("ins2") = forAll { (a: Int, b: Int) => 
    val min = scala.math.min(a, b)
    val h = insert(a, empty)
    val h2 = insert(b, h)
    findMin(h2) == min
  }
  
  // proves heap5 wrong
  property("meld1") = forAll { (h1: H, h2: H) =>
    val h1min = if (isEmpty(h1)) 0 else findMin(h1)
    val h2min = if (isEmpty(h2)) 0 else findMin(h2)
    val melded = meld(h1, h2)
    findMin(melded) == h1min || findMin(melded) == h2min
  }
  
  property("ordered1") = forAll { h: H =>
    checkHeapOrder(h)
  }
  
  property("meld_ordered") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    checkHeapOrder(melded)
  }
  
  property("ordered_after_delete") = forAll { h: H =>
    if(isEmpty(h)) true
    val del = deleteMin(h)
    checkHeapOrder(del)
  }
  
  property("ordered_after_insert") = forAll { (h: H, a: Int) =>
    val ins = insert(a, h)
    checkHeapOrder(ins)
  }
  
  property("ordered_after_insert_and_delete") = forAll { (h: H, a: Int) =>
    val ins = insert(a, h)
    val del = deleteMin(ins)
    checkHeapOrder(del)
  }
  
  lazy val genHeap: Gen[H] = {
    for {
      x <- arbitrary[Int]
      m <- oneOf(value(empty), genHeap)
    } yield insert(x, m)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
