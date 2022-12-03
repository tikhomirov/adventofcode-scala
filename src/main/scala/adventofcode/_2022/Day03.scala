package adventofcode._2022

import scala.io.Source

object Day03:
  lazy val source = Source.fromResource("2022/day03/input")
  lazy val rucksacks = source.getLines().map(parse)

  def parse(line: String): Vector[Int] =
    line.stripLineEnd.toCharArray.map {
      case char if char <= 'Z' => char - 'A' + 27
      case char => char - 'a' + 1
    }.toVector

  def part1: Int =
    rucksacks
      .map(rucksack => rucksack.grouped(rucksack.length / 2))
      .flatMap(_.reduce(_.toSet & _.toSet))
      .sum

  def part2: Int =
    rucksacks
      .grouped(3)
      .flatMap(_.reduce(_.toSet & _.toSet))
      .sum

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
