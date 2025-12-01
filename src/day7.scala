package day7

import util._

@main def main(args: String*): Unit =
  val lines = readLines(inputFile(args *))
  val start = lines.head.indexOf('S')
  val manifold = lines.drop(1).map: line =>
    line.zipWithIndex.filter((ch, pos) => ch == '^').map(_._2).toSet
  .filter(_.nonEmpty)
  part1:
    var beams = Set[Int](start)
    var next = Set[Int]()
    var splits = 0
    for splitters <- manifold do
      for beam <- beams do
        if splitters.contains(beam) then
          splits += 1
          next += beam - 1
          next += beam + 1
        else
          next += beam
      beams = next
      next = Set[Int]()
    splits
  part2:
    val cache = Map[(Int, Int), Long]()

    def count(level: Int, beam: Int): Long =
      def memoize(f: => Long) =
        if cache.contains((level, beam)) then
          cache((level, beam))
        else
          val result = f
          cache((level, beam)) = result
          result

      if level >= manifold.length then
        1
      else if manifold(level).contains(beam) then
        memoize:
          count(level + 1, beam - 1) + count(level + 1, beam + 1)
      else
        memoize:
          count(level + 1, beam)

    count(0, start)