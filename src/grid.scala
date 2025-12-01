package grid

import scala.reflect.ClassTag

private val adjPos: IndexedSeq[(Int, Int)] =
  for
    dr <- -1 to 1
    dc <- -1 to 1
    if (dr | dc) != 0
  yield (dr, dc)

private val directAdjPos: IndexedSeq[(Int, Int)] =
  for
    dr <- -1 to 1
    dc <- -1 to 1
    if (dr != 0 && dc == 0) || (dr == 0 && dc == 0)
  yield (dr, dc)

object Grid:
  def fromLines(lines: IndexedSeq[String]): Grid[Char] =
    Grid[Char](lines.size, lines.head.length):
      (row, col) => lines(row)(col)

  def bitsFromLines(lines: IndexedSeq[String]): Grid[Boolean] =
    Grid[Boolean](lines.size, lines.head.length):
      (row, col) => lines(row)(col) != '.'

class Grid[T: ClassTag](val rows: Int, val cols: Int)(init: (Int, Int) => T):
  val data: Array[Array[T]] = Array.tabulate(rows, cols)(init)

  def eachRow: Seq[Int] = 0 until rows

  def eachCol: Seq[Int] = 0 until cols

  def apply(row: Int, col: Int): T =
    data(row)(col)

  def update(row: Int, col: Int, value: T): Unit =
    data(row)(col) = value

  def adjacentPositions(row: Int, col: Int): Seq[(Int, Int)] =
    for
      (dr, dc) <- adjPos
      if (0 until rows).contains(row + dr) && (0 until cols).contains(col + dc)
    yield (row + dr, col + dc)

  def adjacent(row: Int, col: Int): Seq[T] =
    for (r, c) <- adjacentPositions(row, col) yield data(r)(c)

  def directAdjacentPositions(row: Int, col: Int): Seq[(Int, Int)] =
    for
      (dr, dc) <- directAdjPos
      if (0 until rows).contains(row + dr) && (0 until cols).contains(col + dc)
    yield (row + dr, col + dc)

  def directAdjacent(row: Int, col: Int): Seq[T] =
    for (r, c) <- directAdjacentPositions(row, col) yield data(r)(c)





