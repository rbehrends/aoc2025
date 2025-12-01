package day3

import util._

@main def main(args: String*): Unit =
  val lines = readLines(inputFile(args *))
  part1:
    lines.map: line =>
      val nums = line.toCharArray.map(_.asDigit)
      val (a, pos) = nums.view.dropRight(1).zipWithIndex.maxBy(_._1)
      val b = nums.view.drop(pos + 1).max
      a * 10 + b
    .sum
  part2:
    lines.map: line =>
      val digits = ArrayBuffer[Long]()
      val nums = line.toCharArray.map(_.asDigit)
      var start = 0
      for i <- 11 to 0 by -1 do
        val (digit, pos) = nums.view.drop(start).dropRight(i).zipWithIndex.maxBy(_._1)
        start += pos + 1
        digits += digit
      digits.fold(0L)(_ * 10 + _)
    .sum
