package day1

import util._

@main def main(args: String*): Unit =
  val lines = readLines(inputFile(args *))
  part1:
    var pos = 50
    var password = 0
    for line <- lines do
      val delta = line.substring(1).toInt
      if line.startsWith("L") then
        pos -= delta
      else
        pos += delta
      if pos % 100 == 0 then
        password += 1
    password
  part2:
    var pos = 50
    var password = 0
    for line <- lines do
      var delta = line.substring(1).toInt
      password += delta / 100
      delta %= 100
      if line.startsWith("L") then
        if pos != 0 && delta >= pos then
          password += 1
        pos -= delta
        if pos < 0 then
          pos += 100
      else
        pos += delta
        if pos >= 100 then
          pos -= 100
          password += 1
    password