package adventofcode._2022

import scala.io.Source

object Day04:
  lazy val input = Source.fromResource("2022/day04/input")
  lazy val ranges = input
    .getLines()
    .map(_.split(',').map(parseRange))
    .map { case Array(a, b) => a -> b }
    .toSeq

  def parseRange(s: String): (Int, Int) =
    val Array(start, end) = s.split('-').map(_.toInt)
    start -> end

  def included(a: (Int, Int), b: (Int, Int)): Boolean =
    a._1 >= b._1 && a._2 <= b._2

  def overlap(a: (Int, Int), b: (Int, Int)): Boolean =
    (a._1 >= b._1 && a._1 <= b._2) || (a._2 <= b._2 && a._2 >= b._1)

  def part1: Int =
    ranges.count((a, b) => included(a, b) || included(b, a))

  def part2: Int =
    ranges.count((a, b) => overlap(a, b) || overlap(b, a))

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)