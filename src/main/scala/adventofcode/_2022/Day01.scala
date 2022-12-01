package adventofcode._2022

import scala.io.Source

object Day01:
  lazy val source = Source.fromResource("2022/day01/input")
  lazy val input = source.getLines().map(_.toIntOption).toSeq

  def caloriesPerElv(calories: Seq[Option[Int]]): Seq[Int] =
    calories.foldLeft(Seq(0)) { (acc, value) =>
      value match {
        case Some(i) => (acc.head + i) +: acc.tail
        case None => 0 +: acc
      }
    }

  def part1: Int =
    caloriesPerElv(input).max

  def part2: Int =
    caloriesPerElv(input).sorted.takeRight(3).sum

  def main(argv: Array[String]): Unit =
    println(part1)
    println(part2)
