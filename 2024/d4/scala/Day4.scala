package bt.aoc2024.d1

import cats.syntax.all.*
import cats.effect.*
import cats.effect.std.Console
import cats.effect.kernel.Sync
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.*
import scala.util.control.Breaks.*
import fs2.io.file.{Files, Path}
import cats.Apply
import cats.Applicative
import scala.annotation.tailrec

object Day4 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d4/input.txt"

  private def readLists[F[_]: Files: Concurrent]: F[Array[Array[Char]]] =
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.toCharArray())
      .compile
      .toVector
      .map(_.toArray)

  final case class Matrix(aoa: Array[Array[Char]]):
    lazy val xSize = aoa(0).size
    lazy val ySize = aoa.size
    def get(point: Vector): Option[Char] =
      if (0 <= point.x && point.x < xSize && 0 <= point.y && point.y < ySize)
        Some(aoa(point.y)(point.x))
      else None

  final case class Vector(x: Int, y: Int):
    def +(v: Vector): Vector =
      this.copy(x + v.x, y + v.y)
    def -(v: Vector): Vector =
      this.copy(x - v.x, y - v.y)

  private val allDirections = Seq(
    Seq((-1, -1), (-1, 0), (-1, 1)),
    Seq((0, -1), (0, 1)),
    Seq((1, -1), (1, 0), (1, 1))
  ).flatMap(_.map(Vector.apply(_, _)))

  private val xDirections = Seq(
    Seq((-1, -1), (-1, 1)),
    Seq((1, -1), (1, 1))
  ).flatMap(_.map(Vector.apply(_, _)))

  def firstTask[F[_]: Applicative: Console](matrix: Matrix): F[Int] =
    val pattern = "XMAS"
    @tailrec
    def findPattern(pattern: String, position: Vector, shift: Vector): Int =
      val nextPosition = position + shift
      if (pattern.nonEmpty) {
        if (matrix.get(nextPosition).exists(_ == pattern(0))) {
          findPattern(pattern.tail, nextPosition, shift)
        } else 0
      } else 1

    Applicative[F].pure {
      val firstChar = pattern(0)
      matrix.aoa.zipWithIndex.map { case (xArray, y) =>
        xArray.zipWithIndex
          .filter { case (x, _) => x == firstChar }
          .map { case (char, x) =>
            val currentPosition = Vector(x, y)
            allDirections.map { shift =>
              findPattern(pattern.tail, currentPosition, shift)
            }.sum
          }
          .sum
      }.sum
    }

  def secondTask[F[_]: Applicative: Console](matrix: Matrix): F[Int] =
    val pattern = "MAS"
    Applicative[F].pure {
      matrix.aoa.zipWithIndex
        .filterNot { case (_, l) => l == 0 || l == matrix.ySize }
        .map { case (xArray, y) =>
          xArray.zipWithIndex
            .filterNot { case (_, l) => l == 0 || l == matrix.xSize }
            .filter { case (x, _) => x == pattern(1) }
            .map { case (_, x) =>
              val aPosition = Vector(x, y)
              xDirections
                .flatMap { shift =>
                  matrix
                    .get(aPosition + shift)
                    .filter(_ == pattern(0))
                    .flatMap(_ => matrix.get(aPosition - shift))
                    .filter(_ == pattern(2))
                    .map(_ => 0.5)
                }
                .sum
                .toInt
            }
            .sum
        }
        .sum
    }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      matrix <- readLists[IO]
      firstResult <- firstTask[IO](Matrix(matrix))
      _ <- IO.println(s"1st Task: XMAS occure: $firstResult")
      secondResult <- secondTask[IO](Matrix(matrix))
      _ <- IO.println( s"2nd Taks: X-MAS occurs: $secondResult" )
    } yield ExitCode.Success
}
