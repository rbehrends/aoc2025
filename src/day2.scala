package day2

import util._

def digits(n: Long): Int =
  var a = n
  var result = 1
  while a >= 10 do
    a /= 10
    result += 1
  result

def pow10(n: Int): Long =
  var result = 1L
  for i <- 1 to n do
    result *= 10
  result

val exp10 = (0 to 19).map(pow10).toArray

def segments(lo: Long, hi: Long): Seq[(Long, Long)] =
  val d = digits(lo)
  val end = pow10(d)
  if end > hi then
    Seq((lo, hi))
  else
    Seq((lo, end - 1)) ++ segments(end, hi)

val maxDigits = 20
val divisors =
  val result = Array.fill(maxDigits) {
    ArrayBuffer[Long]()
  }
  for i <- 1 to maxDigits do
    for j <- 1 to i / 2 do
      if i % j == 0 then
        var t = 0L
        for k <- 0 until i by j do
          t += exp10(k)
        result(i - 1) += t
  result.map(_.toArray)

def countTwiceRepeated(lo: Long, hi: Long): Long =
  require(digits(lo) == digits(hi))
  val numDigits = digits(lo)
  if numDigits % 2 == 1 then
    return 0
  val step = exp10(numDigits / 2) + 1
  val start = (lo + step - 1) / step * step
  // Scala already uses Gauss summation to calculate the sum
  // a range, so this takes O(1) time.
  (start to hi by step).sum

def anyRepeats(n: Long): Boolean =
  val d = digits(n)
  divisors(d - 1).exists(k => n % k == 0)

def countAnyRepeated(lo: Long, hi: Long): Long =
  require(digits(lo) == digits(hi))
  val numDigits = digits(lo)
  var result = 0L
  for step <- divisors(numDigits - 1) do
    val patmin = (lo + step - 1) / step
    val patmax = hi / step
    for pat <- patmin to patmax do
      if !anyRepeats(pat) then
        result += pat * step
  result

@main def main(args: String*): Unit =
  val numbers = extractLongNumbers(readFile(inputFile(args *)))
  val ranges = numbers.grouped(2).map { pair => (pair(0), pair(1)) }.toArray
  part1:
    var invalid = 0L
    for (lo, hi) <- ranges do
      for (seglo, seghi) <- segments(lo, hi) do
        invalid += countTwiceRepeated(seglo, seghi)
    invalid
  part2:
    var invalid = 0L
    for (lo, hi) <- ranges do
      for (seglo, seghi) <- segments(lo, hi) do
        invalid += countAnyRepeated(seglo, seghi)
    invalid
