package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import u05lab.ex1.List.*

class ListTest:
  private val l = 10 :: 25 :: 30 :: 15 :: Nil()

  @Test
  def testZipRightRecursive(): Unit =
    assertEquals((10, 0) :: (25, 1) :: (30, 2) :: (15, 3) :: Nil(), l.zipRightRecursive)

  @Test
  def testZipRight(): Unit =
    assertEquals((10, 0) :: (25, 1) :: (30, 2) :: (15, 3) :: Nil(), l.zipRight)

  @Test
  def testPartitionRecursive(): Unit =
    assertEquals((10 :: 30 :: Nil(), 25 :: 15 :: Nil()), l.partitionRecursive(_ % 2 == 0))

  @Test
  def testPartition(): Unit =
    assertEquals((10 :: 30 :: Nil(), 25 :: 15 :: Nil()), l.partition(_ % 2 == 0))

  @Test
  def testSpanRecursive(): Unit =
    assertEquals((10 :: 25 :: Nil(), 30 :: 15 :: Nil()), l.spanRecursive(_ <= 25))

  @Test
  def testSpanRecursiveZeroElements(): Unit =
    assertEquals((Nil(), 10 :: 25 :: 30 :: 15 :: Nil()), l.spanRecursive(_ < 0))

  @Test
  def testSpan(): Unit =
    assertEquals((10 :: 25 :: Nil(), 30 :: 15 :: Nil()), l.span(_ <= 25))

  @Test
  def testReduceRecursive(): Unit =
    assertEquals(80, l.reduceRecursive(_ + _))

  @Test
  def testReduce(): Unit =
    assertEquals(80, l.reduce(_ + _))

  @Test
  def testReduceExc(): Unit =
    assertThrows(classOf[UnsupportedOperationException], () => Nil().reduce((a, b) => a))

  @Test
  def testTakeRightRecursive(): Unit =
    assertEquals(25 :: 30 :: 15 :: Nil(), l.takeRightRecursive(3))

  @Test
  def testTakeRight(): Unit =
    assertEquals(25 :: 30 :: 15 :: Nil(), l.takeRight(3))

  @Test
  def testCollect(): Unit =
    assertEquals(26 :: 31 :: Nil(), l.collect({case i if i > 15 => i + 1}))
