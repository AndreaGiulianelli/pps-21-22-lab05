package u05lab.ex1

import org.junit.Assert.assertEquals
import org.junit.Test
import u05lab.ex1.List.*

class ListTest {
  private val l = 10 :: 25 :: 30 :: 35 :: Nil()

  @Test
  def testZipRightRecursive(): Unit =
    assertEquals((10, 0) :: (25, 1) :: (30, 2) :: (35, 3) :: Nil(), l.zipRightRecursive)

  @Test
  def testZipRight(): Unit =
    assertEquals((10, 0) :: (25, 1) :: (30, 2) :: (35, 3) :: Nil(), l.zipRight)

  @Test
  def testPartitionRecursive(): Unit =
    assertEquals((10 :: 30 :: Nil(), 25 :: 35 :: Nil()), l.partitionRecursive(_ % 2 == 0))

  @Test
  def testPartition(): Unit =
    assertEquals((10 :: 30 :: Nil(), 25 :: 35 :: Nil()), l.partition(_ % 2 == 0))

  @Test
  def testSpanRecursive(): Unit =
    assertEquals((10 :: 25 :: Nil(), 30 :: 35 :: Nil()), l.spanRecursive(_ <= 25))

  @Test
  def testSpan(): Unit =
    assertEquals((10 :: 25 :: Nil(), 30 :: 35 :: Nil()), l.span(_ <= 25))


}
