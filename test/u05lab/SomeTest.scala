package u05lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

import code.List

class SomeTest {

  @Test
  def testIncremental() {
    assert(true)
  }

  @Test
  def testZipRight(): Unit = {
    val l = List("a", "b", "c")

    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0), ("b",1), ("c",2)), l.zipRight)
  }

  @Test
  def testPartition: Unit = {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assertEquals((List(1,2,3,4,5), List(6,7,8,9,10)), l.partition(_ < 6))
  }

  @Test
  def testSpan: Unit = {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assertEquals((List(1,2,3,4,5), List(6,7,8,9,10)), l.span(_ <= 5))
    assertEquals((List(1), List(2,3,4,5,6,7,8,9,10)), l.span(_ % 2 == 1))
  }

  @Test
  def testReduce: Unit = {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val h = List(1)
    val empty: List[Int] = List.nil

    assertEquals(55, l.reduce(_+_))
    assertEquals(1, h.reduce(_+_))
    //assertThrows(new UnsupportedOperationException, empty.reduce(_+_))
  }
}