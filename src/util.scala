package util

import scala.util.matching.Regex
import scala.util.Using
import scala.io.Source
import scala.util.boundary, boundary.break

val Map = scala.collection.mutable.Map
type Map[K, V] = scala.collection.mutable.Map[K, V]

val Set = scala.collection.mutable.Set
type Set[T] = scala.collection.mutable.Set[T]

val ArrayBuffer = scala.collection.mutable.ArrayBuffer
type ArrayBuffer[T] = scala.collection.mutable.ArrayBuffer[T]

private val numRegex = Regex(raw"\d+")
private val signedNumRegex = Regex(raw"[-+]?\d+")
private val tokenRegex = Regex(raw"\S+")
private val wordRegex = Regex(raw"\w+")

def readLines(filename: String): Array[String] =
  Using(Source.fromFile(filename))(_.getLines.toArray).get

def readFile(filename: String): String =
  Using(Source.fromFile(filename))(_.mkString).get

def readRecords(filename: String): Array[String] =
  readFile(filename).split("\n\n")

def extractNumbers(text: String): Array[Int] =
  numRegex.findAllIn(text).map(s => s.toInt).toArray

def extractLongNumbers(text: String): Array[Long] =
  numRegex.findAllIn(text).map(s => s.toLong).toArray

def extractSignedNumbers(text: String): Array[Int] =
  signedNumRegex.findAllIn(text).map(s => s.toInt).toArray

def extractSignedLongNumbers(text: String): Array[Long] =
  signedNumRegex.findAllIn(text).map(s => s.toLong).toArray

def extractTokens(text: String): Array[String] =
  tokenRegex.findAllIn(text).toArray

def extractWords(text: String): Array[String] =
  wordRegex.findAllIn(text).toArray

private def part[T](n: Int, body: => T): Unit =
  val start = System.nanoTime()
  val result = body
  val end = System.nanoTime()
  println(f"part $n: $result (${(end - start) / 1e6}%.3f ms)")

def part1[T](body: => T): Unit =
  part(1, body)

def part2[T](body: => T): Unit =
  part(2, body)

def inputFile(args: String*): String =
  if args.isEmpty then
    f"input/input${day()}.txt"
  else
    args(0)

private def currentFile(): String =
  val stackTrace = Thread.currentThread().getStackTrace
  boundary:
    for i <- 1 until stackTrace.length do
      val frame = stackTrace(i)
      val file = frame.getFileName
      if file.startsWith("day") && file.endsWith(".scala") then
        break(file)
    throw IllegalStateException("stack trace does not contain day*.scala file")

private def day(): String =
  numRegex.findFirstIn(currentFile()).get
