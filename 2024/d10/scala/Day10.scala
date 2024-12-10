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

object Day10 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d10/input.txt"

  private def readLists[F[_]: Files: Concurrent]: F[Array[Array[Int]]] =
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.toCharArray().map(c => (c - '0').toInt))
      .compile
      .toVector
      .map(_.toArray)

  final case class Matrix(aoa: Array[Array[Int]]):
    lazy val xSize = aoa(0).size
    lazy val ySize = aoa.size
    def get(point: Vector): Option[Int] =
      if (0 <= point.x && point.x < xSize && 0 <= point.y && point.y < ySize)
        Some(aoa(point.y)(point.x))
      else None

  final case class Vector(x: Int, y: Int):
    def +(v: Vector): Vector =
      this.copy(x + v.x, y + v.y)

  private val directions = Set(
    (-1, 0), (0, -1), (0, 1), (1, 0)
  ).map(Vector.apply(_, _))

  def firstTask(matrix: Matrix): Int =

    def findPattern(next: Int, position: Vector): Set[Vector] =
      directions.flatMap { shift =>
        if (next <= 9) {
          val nextPosition = position + shift
          if (matrix.get(nextPosition).exists(_ == next)) {
            findPattern(next + 1, nextPosition)
          } else Set.empty
        } else Set(position)
      }

    val seelevel = 0
    matrix.aoa.zipWithIndex.flatMap { case (xArray, y) =>
      xArray.zipWithIndex
        .filter { case (height, _) => height == seelevel }
        .map { case (_, x) =>
          val currentPosition = Vector(x, y)
          currentPosition -> findPattern(seelevel + 1, currentPosition)
        }
        .toSet
    }.toSeq.map(_._2.size).sum

  def secondTask(matrix: Matrix): Int =
    def findPattern(next: Int, position: Vector): Seq[Vector] =
      directions.toSeq.flatMap { shift =>
        if (next <= 9) {
          val nextPosition = position + shift
          if (matrix.get(nextPosition).exists(_ == next)) {
            // println(s"look arround $nextPosition for the $next")
            findPattern(next + 1, nextPosition)
          } else Seq.empty
        } else Seq(position)
      }

    val seelevel = 0
    val r = matrix.aoa.zipWithIndex.flatMap { case (xArray, y) =>
      xArray.zipWithIndex
        .filter { case (height, _) => height == seelevel }
        .map { case (_, x) =>
          val currentPosition = Vector(x, y)
          currentPosition -> findPattern(seelevel + 1, currentPosition)
        }
    }
    // r.foreach{case (start, ends) => println(s"$start - ${ends.mkString}")}
    r.foreach{case (start, ends) => println(s"$start - ${ends.size}")}
    r.map(_._2.size).sum/4

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      matrix <- readLists[IO]
      firstResult = firstTask(Matrix(matrix))
      _ <- IO.println(s"1st Task: $firstResult")
      secondResult = secondTask(Matrix(matrix))
      _ <- IO.println( s"2nd Taks: $secondResult" )
    } yield ExitCode.Success
}
