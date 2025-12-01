package day8

import util._

import scala.util.boundary
import boundary.break
import scala.reflect.ClassTag

type Point = (Long, Long, Long)

def sqr(x: Long): Long = x * x

def squaredDistance(p1: Point, p2: Point): Long =
  sqr(p1._1 - p2._1) + sqr(p1._2 - p2._2) + sqr(p1._3 - p2._3)

def computeDistances(points: Array[Point]): Array[(Long, Point, Point)] =
  val distances = ArrayBuffer[(Long, Point, Point)]()
  for i <- points.indices do
    for j <- i + 1 until points.length do
      val p1 = points(i)
      val p2 = points(j)
      val d = squaredDistance(p1, p2)
      distances += ((d, p1, p2))
  distances.sortWith(_._1 < _._1).toArray

class UnionFind[T: ClassTag]:
  private val index: Map[T, Int] = Map[T, Int]()
  private val reverse: ArrayBuffer[T] = ArrayBuffer[T]()
  private val parent: ArrayBuffer[Int] = ArrayBuffer[Int]()
  private val size: ArrayBuffer[Int] = ArrayBuffer[Int]()
  private var currentIndex = 0
  private var componentCount = 0

  def numComponents: Int =
    componentCount

  def numItems: Int =
    reverse.size

  private def findIndex(i: Int): Int =
    if parent(i) != i then
      parent(i) = findIndex(parent(i))
    parent(i)

  private def findOrAdd(item: T): Int =
    var result = index.getOrElse(item, -1)
    if result < 0 then
      result = currentIndex
      index(item) = currentIndex
      reverse += item
      parent += currentIndex
      size += 1
      currentIndex += 1
      componentCount += 1
    result

  def find(item: T): T =
    reverse(findIndex(findOrAdd(item)))

  def union(item1: T, item2: T): Unit =
    val root1 = findIndex(findOrAdd(item1))
    val root2 = findIndex(findOrAdd(item2))

    if root1 == root2 then
      return

    if size(root1) < size(root2) then
      parent(root1) = root2
      size(root2) += size(root1)
    else
      parent(root2) = root1
      size(root1) += size(root2)
    componentCount -= 1

  def components: Array[Array[T]] =
    reverse.groupBy { x => findIndex(index(x)) }.values.map(_.toArray).toArray

class NaiveUnionFind[T: ClassTag]:
  private val idMap: Map[T, Int] = Map()
  private var currentId: Int = 0
  private var componentCount: Int = 0

  def components: Array[Array[T]] =
    idMap.keys.groupBy(idMap(_)).values.map(_.toArray).toArray

  def numItems: Int =
    idMap.size

  def numComponents: Int =
    componentCount

  def union(item1: T, item2: T): Unit =
    if idMap.contains(item1) then
      if idMap.contains(item2) then
        val id1 = idMap(item1)
        val id2 = idMap(item2)
        if id1 != id2 then
          for (item, id) <- idMap do
            if id == id2 then
              idMap(item) = id1
          componentCount -= 1
      else
        idMap(item2) = idMap(item1)
    else if idMap.contains(item2) then
      idMap(item1) = idMap(item2)
    else
      idMap(item1) = currentId
      idMap(item2) = currentId
      currentId += 1
      componentCount += 1

@main def main(args: String*): Unit =
  val points = readLines(inputFile(args *)).map: line =>
    val p = extractLongNumbers(line)
    (p(0), p(1), p(2))
  val numConnections = if (points.length <= 20) 10 else 1000
  part1:
    val distances = computeDistances(points)
    val circuits = UnionFind[Point]()
    for (_, p1, p2) <- distances.take(numConnections) do
      circuits.union(p1, p2)
    circuits.components.map(_.length).sortBy(-_).take(3).product
  part2:
    val distances = computeDistances(points)
    val circuits = UnionFind[Point]()
    val (last1, last2) = boundary:
      var i = 0
      for (_, p1, p2) <- distances do
        i += 1
        circuits.union(p1, p2)
        if circuits.numComponents == 1 && circuits.numItems == points.length then
          break((p1._1, p2._1))
      break((0L, 0L))
    last1 * last2