package sandbox

import scala.collection.mutable

final case class Diff(diff: Int, p1: Int, p2: Int)

object ProjectEuler44:

  val maxPentagon = 10000

  def pentagon(n: Int): Int = n * (3 * n - 1) / 2

  def isPentagon(n: Int): Boolean = false

  def solve(): Option[Diff] = firstImperative()

  def firstImperative(): Option[Diff] =
    val lookup = mutable.Set[Int]()
    val pentagons = new Array[Int](maxPentagon)

    for n <- 1.to(maxPentagon) do
      val pn = pentagon(n)
      lookup.add(pn)
      pentagons(n - 1) = pn

    val candidates = mutable.Set[Diff]()

    for p1 <- pentagons do
      for p2 <- pentagons do
        val diff = Math.abs(p1 - p2)
        if p1 != p2 && lookup.contains(p1 + p2) && lookup.contains(diff) then
          candidates.add(Diff(diff, p1, p2))

    candidates.toList.sortBy(_.diff).toList.headOption

  def imperativeMaxDistance(): Option[Diff] =
    val lookup = mutable.Set[Int]()
    val pentagons = new Array[Int](maxPentagon)

    for n <- 1.to(maxPentagon) do
      val pn = pentagon(n)
      lookup.add(pn)
      pentagons(n - 1) = pn

    val candidates = mutable.Set[Diff]()
    var maxPossibleDistance = maxPentagon

    for p1Index <- 1.until(maxPentagon) do
      val p1 = pentagons(p1Index)
      
      for p2Index <- p1Index.until(Math.min(p1Index + maxPossibleDistance, maxPentagon)) do
        val p2 = pentagons(p2Index)
        val diff = Math.abs(p1 - p2)

        if p1 != p2 && lookup.contains(p1 + p2) && lookup.contains(diff) then
          candidates.add(Diff(diff, p1, p2))
          maxPossibleDistance = Math.abs(p1Index - p2Index)
      
    candidates.toList.sortBy(_.diff).toList.headOption

  def imperativeMaxDistanceHashLookup(): Option[Diff] =
    val lookup = mutable.HashSet[Int]()
    val pentagons = new Array[Int](maxPentagon)

    for n <- 1.to(maxPentagon) do
      val pn = pentagon(n)
      lookup.add(pn)
      pentagons(n - 1) = pn

    val candidates = mutable.Set[Diff]()
    var maxPossibleDistance = maxPentagon

    for p1Index <- 1.until(maxPentagon) do
      val p1 = pentagons(p1Index)

      for p2Index <- p1Index.until(Math.min(p1Index + maxPossibleDistance, maxPentagon)) do
        val p2 = pentagons(p2Index)
        val diff = Math.abs(p1 - p2)

        if p1 != p2 && lookup.contains(p1 + p2) && lookup.contains(diff) then
          candidates.add(Diff(diff, p1, p2))
          maxPossibleDistance = Math.abs(p1Index - p2Index)

    candidates.toList.sortBy(_.diff).toList.headOption

def timed[A](f: => A): (Long, A) =
  val startTime = System.nanoTime()
  val result = f
  val endTime = System.nanoTime()
  (endTime - startTime, result)
  
final case class Timing(var avg: Long, var min: Long, var max: Long):
  def nanoToMillis(nano: Long): Double = nano.toDouble / 1000000.0
  def pp(): String =
    s"""Avg: ${nanoToMillis(avg)}ms
       |Min: ${nanoToMillis(min)}ms
       |Max: ${nanoToMillis(max)}ms""".stripMargin
  
def timedN[A](f: => A, n: Int): Timing =
  assert(n > 0)
  val firstTime = timed(f)._1
  val timing = Timing(firstTime, firstTime, firstTime)
  for _ <- 1.until(n) do
    val time = timed(f)._1
    timing.avg += time
    if time < timing.min then
      timing.min = time
    if time > timing.max then
      timing.max = time
  timing.avg = timing.avg / n
  timing

@main
def benchmarkProjectEuler44Solutions(): Unit =
  timedN(ProjectEuler44.imperativeMaxDistance(), 10)
  timedN(ProjectEuler44.imperativeMaxDistanceHashLookup(), 10)
  
  val firstImperativeTime = timedN(ProjectEuler44.firstImperative(), 1)
  println(s"First Imperative:\n${firstImperativeTime.pp()}")
  
  val imperativeMaxDistance = timedN(ProjectEuler44.imperativeMaxDistance(), 10)
  println(s"Imperative with max distance:\n${imperativeMaxDistance.pp()}")

  val imperativeMaxDistanceHashLookup = timedN(ProjectEuler44.imperativeMaxDistanceHashLookup(), 10)
  println(s"Imperative with max distance:\n${imperativeMaxDistanceHashLookup.pp()}")
