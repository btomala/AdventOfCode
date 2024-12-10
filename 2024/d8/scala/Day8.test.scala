import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import bt.aoc2024.d1.Day8.*
import cats.effect.*
import cats.Id

class MatrixSpec extends AnyFlatSpec with Matchers {
  // Arrange
  val initialMatrix = Matrix(
    """
    |............
    |........0...
    |.....0......
    |.......0....
    |....0.......
    |......A.....
    |............
    |............
    |........A...
    |.........A..
    |............
    |............
    |""".stripMargin.split("\n").filter(_.nonEmpty).map(_.toCharArray())
  )

  "firstTask" should "return correct value" in {

    // Act
    val numberOfAntinodes = firstTask(initialMatrix)

    // Assert
    numberOfAntinodes.shouldEqual(14)
  }

  "secondTask" should "return correct value" in {

    // Act
    val numberOfAntinodes = secondTask(initialMatrix)

    // Assert
    numberOfAntinodes.shouldEqual(34)
  }
}
