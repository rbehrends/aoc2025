package day10

import util.*

import scala.util.boundary
import boundary.break
import scala.annotation.tailrec

case class Schematic(target: Int, buttons: Array[Int])

case class Schematic2(target: Array[Int], buttons: Array[Array[Int]])

@tailrec
def gcd(a: Long, b: Long): Long =
  if a < 0 || b < 0 then
    gcd(a.abs, b.abs)
  else if b == 0 then
    a
  else
    gcd(b, a % b)

def makeUpperTriangular(mat: Array[Array[Long]]): Unit =
  // Basically, we're converting the matrix to a row echelon form
  // with positive values on the diagonal.
  val nRows = mat.length
  val nCols = mat.head.length - 1 // Last column is the constant vector

  var pivotRow = 0

  for col <- 0 until nCols do
    // Find a row with a non-zero value in this column
    (pivotRow until nRows).find(row => mat(row)(col) != 0) match
      case Some(row) =>
        // Move pivot row to top of the current block
        if row != pivotRow then
          val tmp = mat(pivotRow)
          mat(pivotRow) = mat(row)
          mat(row) = tmp
        val pivotVal = mat(pivotRow)(col)
        val g = mat(pivotRow).reduce(gcd)
        // Ensure that the pivot value is positive
        if pivotVal < 0 then
          for j <- col until nCols + 1 do
            mat(pivotRow)(j) *= -1
        // Make entries below pivot zero
        for r <- pivotRow + 1 until nRows do
          if mat(r)(col) != 0 then
            val g = gcd(mat(pivotRow)(col), mat(r)(col))
            val pivotFactor = mat(r)(col) / g
            val otherFactor = mat(pivotRow)(col) / g
            for j <- col until nCols + 1 do
              mat(r)(j) = mat(r)(j) * otherFactor - pivotFactor * mat(pivotRow)(j)
        pivotRow += 1
      // No pivot in this column, move to next column.
      // We will swap columns around in the next step
      // to ensure that the diagonal is populated.
      case None => ()

  // Make sure that the diagonal values are all non-zero.
  for row <- mat.indices if row < nCols do
    if mat(row)(row) == 0 then
      mat(row).indexWhere(_ != 0) match
        case -1 => () // No non-zero value in this row
        case col =>
          // Swap columns so that we have a non-zero value on the diagonal
          for i <- mat.indices do
            val tmp = mat(i)(row)
            mat(i)(row) = mat(i)(col)
            mat(i)(col) = tmp

def parseTarget(desc: String): Int =
  Integer.parseInt(desc.slice(1, desc.length - 1).reverse.replace('.', '0').replace('#', '1'), 2)

def parseButtons(desc: String): Int =
  extractNumbers(desc).fold(0): (acc, b) =>
    acc + (1 << b)

def findCombination(schematic: Schematic): Int =
  schematic.buttons.indices.find: k =>
    schematic.buttons.combinations(k).exists: buttons =>
      schematic.target == buttons.fold(0) { (acc, b) => acc ^ b }
  .get

def generateSolution(mat: Array[Array[Long]], vars: Array[Int], numFreeVars: Int): Option[Int] =
  // Given a matrix in row echelon form and an assignment to the free variables,
  // generate a solution. Return None if the solution does not consist of all integral
  // values or if any value is negative. Otherwise, return the sum of the variables.
  val numDepVars = vars.length - numFreeVars
  boundary:
    for i <- 0 until numDepVars do
      val row = mat(numDepVars - 1 - i)
      var sum = row.last
      for j <- 0 until numFreeVars + i do
        sum -= row(row.length - 2 - j) * vars(j)
      val diagVal = row(numDepVars - 1 - i)
      if sum % diagVal != 0 || sum < 0 then
        break(None)
      val nextDepVar = sum / diagVal
      vars(numFreeVars + i) = nextDepVar.toInt
    break(Some(vars.sum))

def populateVars(numFreeVars: Int, numVars: Int, maxTotal: Int): Iterator[Array[Int]] =
  val vars = Array.fill(numVars)(0)

  def populateFrom(start: Int, remainingTotal: Int): Iterator[Array[Int]] =
    if start == numFreeVars then
      // We can avoid the clone() here if the caller does not modify the first
      // `numFreeVars` elements, but the performance gain is negligible.
      Iterator.single(vars.clone())
    else
      (0 to remainingTotal).iterator.flatMap: v =>
        vars(start) = v
        populateFrom(start + 1, remainingTotal - v)

  populateFrom(0, maxTotal)

def findCombination2(schematic: Schematic2): Int =
  val mat = Array.fill[Long](schematic.target.length, schematic.buttons.length + 1)(0)
  for (button, col) <- schematic.buttons.view.zipWithIndex do
    for b <- button do
      mat(b)(col) = 1
  val lastCol = mat.head.length - 1
  for (t, row) <- schematic.target.view.zipWithIndex do
    mat(row)(lastCol) = t
  makeUpperTriangular(mat)

  val numDepVars = mat.count(_.exists(_ != 0))
  val numFreeVars = mat.head.length - 1 - numDepVars
  val maxVal = schematic.target.max
  var minResult = Int.MaxValue

  for vars <- populateVars(numFreeVars, numFreeVars + numDepVars, maxVal) do
    generateSolution(mat, vars, numFreeVars) match
      case Some(n) => minResult = minResult min n
      case None => ()
  assert(minResult != Int.MaxValue, "no solution found")
  minResult

@main def main(args: String*): Unit =
  val lines = readLines(inputFile(args *))
  part1:
    val schematics = lines.map: line =>
      val tokens = extractTokens(line)
      val target = parseTarget(tokens(0))
      val buttons = tokens.view.drop(1).dropRight(1).map(parseButtons).toArray
      Schematic(target, buttons)
    schematics.map: schematic =>
      findCombination(schematic)
    .sum
  part2:
    val schematics = lines.map: line =>
      val tokens = extractTokens(line)
      val target = extractNumbers(tokens.last)
      val buttons = tokens.view.drop(1).dropRight(1).map(extractNumbers).toArray
      Schematic2(target, buttons)
    schematics.map: schematic =>
      findCombination2(schematic)
    .sum