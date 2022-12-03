package adventofcode._2022

import scala.io.Source

object Day02:
  lazy val source = Source.fromResource("2022/day02/input")
  lazy val rounds = source
    .getLines()
    .map(line => (line.charAt(0) - 'A' + 1) -> (line.charAt(2) - 'X' + 1))
    .toSeq
  val winning = Map(1 -> 2, 2 -> 3, 3 -> 1)
  val losing = Map(1 -> 3, 2 -> 1, 3 -> 2)

  def part1: Int =
    rounds.map {
      case (elf, me) if elf == me => 3 + me
      case (elf, me) if losing(me) == elf => 6 + me
      case (_, me) => me
    }.sum

  def part2: Int =
    rounds.map { (elf, result) =>
      result match {
        case 1 => losing(elf)
        case 2 => elf + 3
        case 3 => winning(elf) + 6
      }
    }.sum

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)