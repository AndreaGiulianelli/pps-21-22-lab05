package u05lab.ex3

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

private object PerformanceBenchmarks:
  import PerformanceUtils.*

  def benchLinearISeq() =
    println("< List (immutable) >")
    val l = List(1, 2, 3)
    measure("creation: ")(List(1, 2, 3))
    measure("size: ")(l.size)
    measure("get: ")(l(1))
    measure("add head: ")(0 :: l)
    measure("take first 2: ")(l take 2)

  def benchLinearMSeq() =
    import scala.collection.mutable.ListBuffer
    println("< ListBuffer (mutable) >")
    val l = ListBuffer(1, 2, 3)
    measure("creation: ")(ListBuffer(1, 2, 3))
    measure("size")(l.size)
    measure("get: ")(l(1))
    measure("add head: ")(l prepend 0)
    measure("add tail: ")(l += 4)
    measure("take first 2: ")(l -= 3)

  def benchIndexISeq() =
    println("< Vector (immutable) >")
    val l = Vector(1, 2, 3)
    measure("creation: ")(Vector(1, 2, 3))
    measure("size: ")(l.size)
    measure("get: ")(l(1))
    measure("add head: ")(4 +: l)
    measure("add tail: ")(l :+ 4)
    measure("take first 2: ")(l take 2)

  def benchIndexMSeq() =
    import scala.collection.mutable.ArrayBuffer
    println("< ArrayBuffer (mutable) >")
    val l = ArrayBuffer(1, 2, 3)
    measure("creation: ")(ArrayBuffer(1, 2, 3))
    measure("size")(l.size)
    measure("get: ")(l(1))
    measure("add head: ")(l prepend 0)
    measure("add tail: ")(l += 4)
    measure("take first 2: ")(l -= 3)
    println("< Array (mutable) >")
    val a = Array(1, 2, 3)
    measure("creation: ")(Array(1, 2, 3))
    measure("size")(a.length)
    measure("get: ")(a(1))
    measure("add head: ")(4 +: a)
    measure("take first 2: ")(a take 2)

  def benchISet() =
    println("< Set (immutable) >")
    val s = Set(1, 2, 3)
    measure("creation: ")(Set(1, 2, 3))
    measure("size")(s.size)
    measure("add: ")(s + 4)
    measure("remove: ")(s - 2)

  def benchMSet() =
    import scala.collection.mutable.Set
    println("< Set (mutable) >")
    val s = Set(1, 2, 3)
    measure("creation: ")(Set(1, 2, 3))
    measure("size")(s.size)
    measure("add: ")(s add 4)
    measure("remove: ")(s remove 2)

  def benchIMap() =
    println("< Map (immutable) >")
    val m = Map(1 -> "a", 2 -> "b", 3 -> "c")
    measure("creation: ")(Map(1 -> "a", 2 -> "b", 3 -> "c"))
    measure("size: ")(m.size)
    measure("get: ")(m get 2)
    measure("put: ")(m + (4 -> "d"))
    measure("remove: ")(m - 1)

  def benchMMap() =
    import scala.collection.mutable.Map
    println("< Map (mutable) >")
    val m = Map(1 -> "a", 2 -> "b", 3 -> "c")
    measure("creation: ")(Map(1 -> "a", 2 -> "b", 3 -> "c"))
    measure("size: ")(m.size)
    measure("get: ")(m get 2)
    measure("put: ")(m += 4 -> "d")
    measure("remove: ")(m remove 1)

@main def checkPerformance: Unit =
  import PerformanceBenchmarks.*
  /* Linear sequences: List, ListBuffer */
  benchLinearISeq()
  benchLinearMSeq()
  /* Indexed sequences: Vector, Array, ArrayBuffer */
  benchIndexISeq()
  benchIndexMSeq()
  /* Sets */
  benchISet()
  benchMSet()
  /* Maps */
  benchIMap()
  benchMMap()