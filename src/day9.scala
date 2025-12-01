package day9

import util._

class Point(val x: Long, val y: Long)

class TileList(val ystart: Long, val yend: Long, val intervals: Array[(Long, Long)])

def area(p1: Point, p2: Point): Long =
  ((p2.x - p1.x).abs + 1) * ((p2.y - p1.y).abs + 1)

def insidePolygon(tiles: ArrayBuffer[TileList], rect: (Point, Point)): Boolean =
  val ymin = rect._1.y min rect._2.y
  val ymax = rect._1.y max rect._2.y
  val xmin = rect._1.x min rect._2.x
  val xmax = rect._1.x max rect._2.x
  tiles.view.filter { tilelist => ymin <= tilelist.ystart && tilelist.yend <= ymax }.forall: tilelist =>
    tilelist.intervals.exists((xstart, xend) => xstart <= xmin && xmax <= xend)

@main def main(args: String*): Unit =
  val points = readLines(inputFile(args *)).map(extractLongNumbers).map { p => Point(p(0), p(1)) }
  part1:
    var maxArea = 0L
    for i <- 0 until points.length - 1 do
      for j <- i + 1 until points.length do
        maxArea = maxArea max area(points(i), points(j))
    maxArea
  part2:
    val ycoords = points.map(_.y).distinct.sorted
    val edges = points.zip(points.drop(1) ++ points.take(1))
    val vertEdges = edges.filter { (p1, p2) => p1.x == p2.x }.sortBy(_._1.x)
    val tiles = ArrayBuffer[TileList]()
    for (ystart, yend) <- ycoords.sliding(2).map { ys => (ys(0), ys(1)) } do
      val edgelist = ArrayBuffer[Long]()
      for (p1, p2) <- vertEdges do
        val ymin = p1.y min p2.y
        val ymax = p1.y max p2.y
        if ymin <= ystart && yend <= ymax then
          edgelist += p1.x
      tiles += TileList(ystart, yend, edgelist.grouped(2).map { xs => (xs(0), xs(1)) }.toArray)
    var maxArea = 0L
    for i <- 0 until points.length - 1 do
      for j <- i + 1 until points.length do
        if insidePolygon(tiles, (points(i), points(j))) then
          maxArea = maxArea max area(points(i), points(j))
    maxArea