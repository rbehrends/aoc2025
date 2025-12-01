package day11

import util._

def countPaths(graph: Map[String, Array[String]], start: String, end: String, cache: Map[String, Long] = Map()): Long =
  if start == end then
    1
  else
    if !cache.contains(start) then
      cache(start) = graph(start).iterator.map(countPaths(graph, _, end, cache)).sum
    cache(start)

@main def main(args: String*): Unit =
  val graph = Map.from(readLines(inputFile(args *)).map: line =>
    val words = extractWords(line)
    (words(0), words.drop(1)))
  graph("out") = Array()
  part1:
    countPaths(graph, "you", "out")
  part2:
    // Different results depending on whether dac is reachable from fft or vice versa.
    if countPaths(graph, "fft", "dac") > 0 then
      countPaths(graph, "svr", "fft") * countPaths(graph, "fft", "dac") * countPaths(graph, "dac", "out")
    else
      countPaths(graph, "svr", "dac") * countPaths(graph, "dac", "fft") * countPaths(graph, "fft", "out")
