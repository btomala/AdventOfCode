import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import bt.aoc2024.d1.Day10.*
import cats.effect.*
import cats.Id

class Day10Test extends AnyFlatSpec with Matchers {
  // Arrange
  val initialMap = Matrix("""
    |89010123
    |78121874
    |87430965
    |96549874
    |45678903
    |32019012
    |01329801
    |10456732
    |""".stripMargin.split("\n").filter(_.nonEmpty).map(_.toCharArray().map(c => (c - '0').toInt)).toArray)

  "firstTask" should "return correct value" in {

    // Act
    val result = firstTask(initialMap)

    // Assert
    result.shouldEqual(36)
  }

  "secondTask" should "return correct value" in {

    // Act
    val result = secondTask(initialMap)

    // Assert
    result.shouldEqual(81)
  }
}
