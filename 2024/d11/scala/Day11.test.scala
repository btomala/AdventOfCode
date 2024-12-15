import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import bt.aoc2024.d1.Day11.*
import cats.effect.*
import cats.Id
import cats.effect.unsafe.implicits.global

class Day11Test extends AnyFlatSpec with Matchers {
  // Arrange
  val initialStones = "125 17".split(" ").toArray.map(_.toLong)

  "firstTask" should "return correct value" in {

    // Act
    val result = firstTaskBSF(initialStones, 25)

    // Assert
    result.shouldEqual(55312)
  }

  "secondTask" should "return correct value" ignore {

    // Act
    val result = firstTaskBSF(initialStones, 25)

    // Assert
    result.shouldEqual(55312)
  }
}
