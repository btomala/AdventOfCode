import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import bt.aoc2024.d1.Day6.*
import cats.effect.*
import cats.Id

class MatrixSpec extends AnyFlatSpec with Matchers {

  "markVisited" should "mark the specified point as visited" in {
    // Arrange
    val initialMatrix = Matrix(Array(
      Array('^', '.', '.'),
      Array('.', '#', '.'),
      Array('.', '.', '.')
    ))
    val pointToVisit = Vector(0, 0) // The position of the guard

    // Act
    val updatedMatrix = initialMatrix.markVisited(pointToVisit)

    // Assert
    updatedMatrix.aoa(0)(0).shouldEqual('X') // The guard's position should be marked as visited
    updatedMatrix.aoa(1)(1).shouldEqual('#') // The obstacle should remain unchanged
    updatedMatrix.aoa(2)(2).shouldEqual('.') // Other positions should remain unchanged
  }

  "secondTask" should "mark the specified point as visited" in {
    // Arrange
    val initialMatrix = Matrix(Array(
      "....#.....",
      ".........#",
      "..........",
      "..#.......",
      ".......#..",
      "..........",
      ".#..^.....",
      "........#.",
      "#.........",
      "......#..."
    ).map(_.toCharArray()))

    // Act
    val numberOfObsticles = secondTask(initialMatrix)

    // Assert
    numberOfObsticles.shouldEqual(6)
  }
}
