package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)

    findMin(h) == a
  }

  property("2elem") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val min = Math.min(a, b)

    findMin(h2) == min
  }

  property("del1ElemEmpty") = forAll { (a: Int) =>
    val h = insert(a, empty)

    deleteMin(h) == empty
  }

  property("sortedHeap") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        val newH = deleteMin(h)
        isEmpty(newH) || (min <= findMin(newH) && isSorted(newH))
      }
    isSorted(h)
  }

  property("minOf2heaps") = forAll { (h1: H, h2: H) =>
    val meldH = meld(h1, h2)

    findMin(meldH) == Math.min(findMin(h1), findMin(h2))
  }

  property("checkMeld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }

    val meld1 = meld(h1, h2)
    val minH1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(minH1, h2))

    heapEqual(meld1, meld2)
  }

  property(
    "hint1: If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back."
  ) = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == math.min(a, b)
  }

  property(
    "hint2: If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty."
  ) = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property(
    "hint3: Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima."
  ) = forAll { (h: H) =>
    def getElems(heap: H): List[Int] = {
      isEmpty(heap) match {
        case true => {
          Nil
        }
        case false => {
          val m = findMin(heap)
          val remaining = deleteMin(heap)
          m :: getElems(remaining)
        }
      }
    }
    val elems = getElems(h)
    elems.sorted == elems
  }

  property(
    "hint4: Finding a minimum of the melding of any two heaps should return a minimum of one or the other."
  ) = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val h = meld(h1, h2)
    val m = findMin(h)
    (m == m1) || (m == m2)
  }

  property(
    "Finding a minimum of the melding of a heap with an empty heap should return the minimum of the heap."
  ) = forAll { (h: H) =>
    val m = findMin(h)
    val h1 = meld(h, empty)
    val h2 = meld(empty, h)
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    (m == m1) && (m == m2) && (m1 == m2)
  }

  property(
    "Finding a minimum of the melding of any two identical heaps should return a minimum of one or the other."
  ) = forAll { (h: H) =>
    val m = findMin(h)
    val h1 = meld(h, h)
    val m1 = findMin(h1)
    (m == m1)
  }

  property("Heap sorts") = forAll(Gen.choose(0, 100)) { (n: Int) =>
    def getElems(heap: H): List[Int] = {
      isEmpty(heap) match {
        case true => {
          Nil
        }
        case false => {
          val m = findMin(heap)
          val remaining = deleteMin(heap)
          m :: getElems(remaining)
        }
      }
    }

    def insertList(h: H, list: List[Int]): H = {
      list match {
        case Nil     => h
        case t :: ts => insertList(insert(t, h), ts)
      }
    }
    val toInsert = (1 to n).toList
    val retrievedElems = getElems(insertList(empty, toInsert))
    retrievedElems == toInsert
  }
