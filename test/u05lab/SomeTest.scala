package u05lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import code.List

class SomeTest {

  val stringList = List("a", "b", "c")
  val intList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val singleElementList = List(1)
  val emptyList: List[Int] = List.nil

  @Test
  def testIncremental() {
    assert(true)
  }

  @Test
  def testZipRight(): Unit = {
    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0), ("b",1), ("c",2)), stringList.zipRight)
  }

  @Test
  def testPartition: Unit = {
    assertEquals((List(1,2,3,4,5), List(6,7,8,9,10)), intList.partition(_ < 6))
  }

  @Test
  def testSpan: Unit = {
    assertEquals((List(1,2,3,4,5), List(6,7,8,9,10)), intList.span(_ <= 5))
    assertEquals((List(1), List(2,3,4,5,6,7,8,9,10)), intList.span(_ % 2 == 1))
  }

  @Test
  def testReduce: Unit = {
    assertEquals(55, intList.reduce(_+_))
    assertEquals(1, singleElementList.reduce(_+_))
    //assertThrows(new UnsupportedOperationException, empty.reduce(_+_))
  }

  @Test
  def testTakeRight: Unit = {
    assertEquals(List(8, 9, 10), intList.takeRight(3))
    assertEquals(singleElementList, singleElementList.takeRight(3))
    assertEquals(emptyList, emptyList.takeRight(3))
  }

  @Test
  def testCollect: Unit = {
    assertEquals(List(0,1,2,3,8,9), intList.collect { case x if x<5 || x>8 => x-1 })
  }

}