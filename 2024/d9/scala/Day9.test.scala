import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import bt.aoc2024.d1.Day9.*
import cats.effect.*
import cats.Id

class MemSpec extends AnyFlatSpec with Matchers {
  // Arrange
  val initialMemory = intoMem("2333133121414131402")

  "firstTask" should "return correct value" in {

    // Act
    val numberOfAntinodes = firstTask(initialMemory.flatMap(_.fragments))

    // Assert
    numberOfAntinodes.shouldEqual(1928)
  }

  "secondTask" should "return correct value" in {

    // Act
    val numberOfAntinodes = secondTask(initialMemory)

    // Assert
    numberOfAntinodes.shouldEqual(2858)
  }
}
