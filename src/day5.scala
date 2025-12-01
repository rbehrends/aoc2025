package day5

import util._

import scala.collection.immutable.NumericRange

type Range = NumericRange.Inclusive[Long]

def merge(r1: Range, r2: Range): Range =
  (r1.start min r2.start) to (r1.end max r2.end)

@main def main(args: String*): Unit =
  val parts = readRecords(inputFile(args *))
  val ranges = extractLongNumbers(parts(0)).grouped(2).map { p => p(0) to p(1) }.toArray
  val ingredients = extractLongNumbers(parts(1))
  part1:
    ingredients.count:
      // range.contains() does not work for Long ranges >= Int.MaxValue in in size.
      i => ranges.exists { r => i >= r.start && i <= r.end }
  part2:
    val merged = ArrayBuffer[Range]()
    for range <- ranges.sortBy(_.start) do
      if merged.nonEmpty && range.start <= merged.last.end then
        merged(merged.size - 1) = merge(merged.last, range)
      else
        merged += range
    merged.map { range => range.end - range.start + 1L }.sum