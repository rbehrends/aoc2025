package day12

import util._

@main def main(args: String*): Unit =
  val parts = readRecords(inputFile(args *))
  val sizes = parts.dropRight(1).map: tile =>
    tile.count(_ == '#')
  val regions = parts.last.split('\n').map: line =>
    val nums = extractNumbers(line)
    ((nums(0), nums(1)), nums.drop(2))
  part1:
    regions.count:
      case ((w, h), counts) => w * h >= counts.zip(sizes).map(_ * _).sum