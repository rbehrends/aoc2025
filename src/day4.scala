package day4

import util._
import grid._

@main def main(args: String*): Unit =
  val lines = readLines(inputFile(args *))
  part1:
    val grid = Grid.bitsFromLines(lines)
    var rolls = 0
    for row <- grid.eachRow; col <- grid.eachCol do
      if grid(row, col) then
        var s = 0
        for occupied <- grid.adjacent(row, col) do
          if occupied then
            s += 1
        if s < 4 then
          rolls += 1
    rolls
  part2:
    val grid = Grid.bitsFromLines(lines)
    var changed = true
    var totalRolls = 0
    while changed do
      changed = false
      val rolls = ArrayBuffer[(Int, Int)]()
      for row <- grid.eachRow; col <- grid.eachCol do
        if grid(row, col) then
          var s = 0
          for occupied <- grid.adjacent(row, col) do
            if occupied then
              s += 1
          if s < 4 then
            rolls += ((row, col))
      if rolls.nonEmpty then
        changed = true
        for (row, col) <- rolls do
          grid(row, col) = false
        totalRolls += rolls.size
    totalRolls

