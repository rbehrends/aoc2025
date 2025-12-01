package day6

import util._

@main def main(args: String*): Unit =
  val lines = readLines(inputFile(args *))
  val ops = extractTokens(lines.last).map(_(0))
  part1:
    val nums = lines.view.take(lines.length - 1).map(extractLongNumbers).toArray
    ops.indices.map: i =>
      if ops(i) == '+' then
        nums.view.map(_(i)).sum
      else
        nums.view.map(_(i)).product
    .sum
  part2:
    val columns = lines.view.take(lines.length - 1)
      .map(_.toCharArray).toArray.transpose.map(_.mkString.trim)
      ++ Array("")
    val result = ArrayBuffer[Long]()
    var colIndex = 0
    var opIndex = 0
    val group = ArrayBuffer[Long]()
    while colIndex < columns.length do
      val col = columns(colIndex)
      colIndex += 1
      if col.isEmpty then
        if ops(opIndex) == '+' then
          result += group.sum
        else
          result += group.product
        group.clear()
        opIndex += 1
      else
        group += col.toLong
    assert(group.isEmpty)
    result.sum


