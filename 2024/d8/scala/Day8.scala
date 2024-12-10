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

object Day8 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d8/input.txt"
  private val antinodes = '#'

  private def readLists[F[_]: Files: Concurrent]: F[Array[Array[Char]]] =
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
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

    def markAntinode(point: Vector): Matrix =
      val newRow = aoa(point.y).clone()
      newRow.update(point.x, antinodes)
      val newAoA = aoa.clone()
      newAoA.update(point.y, newRow)
      Matrix(newAoA)

    def setAntinode(point: Vector): Option[Matrix] =
      if (0 <= point.x && point.x < xSize && 0 <= point.y && point.y < ySize) {
        Some(markAntinode(point))
      } else {
        None
      }
    def print() =
      aoa.foreach(xa => println(String(xa)))

  final case class Vector(x: Int, y: Int):
    def +(v: Vector): Vector =
      this.copy(x + v.x, y + v.y)
    def -(v: Vector): Vector =
      this.copy(x - v.x, y - v.y)
    def *(n: Int): Vector =
      this.copy(x*n, y*n)

  private def findAntenas(matrix: Matrix): Map[Char, List[Vector]] =
    matrix.aoa.zipWithIndex
      .flatMap { case (xArray, y) =>
        xArray.zipWithIndex.collect {
          case (c, x) if c.isLetterOrDigit => (c, Vector(x, y))
        }
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2).toList)
      .toMap

  final case class Pair(from: Vector, to: Vector):
    lazy val distance: Vector = to - from
    def allAntinodes(matrix: Matrix): List[Vector] =
      LazyList.from(0)
        .takeWhile( n => matrix.setAntinode(to + distance*n).isDefined)
        .collect{ case n if matrix.setAntinode(to + distance*n).isDefined => to + distance*n }
        .toList ++
      LazyList.from(0)
        .takeWhile( n => matrix.setAntinode(from - distance*n).isDefined)
        .collect{ case n if matrix.setAntinode(from - distance*n).isDefined => from - distance*n }
        .toList

  private def producePair(positions: (Char, List[Vector])): (Char, List[Pair]) =
    positions match {
      case (c, Nil)         => c -> List.empty
      case (c, head :: Nil) => c -> List.empty
      case (c, head :: tail) =>
        val (char, list) = producePair(c -> tail)
        c -> (tail.map(e => Pair(head, e)) ++ list)
    }

  // 364 to high
  def firstTask(matrix: Matrix): Int =
    findAntenas(matrix)
      .map(producePair)
      .map { case (c, list) =>
        c -> list.flatMap(p => List(p.from - p.distance, p.to + p.distance))
      }
      .flatMap { case (c, p) =>
        p.map(v => matrix.setAntinode(v).map(_ => v)).flatten
      }
      .toSet
      .size

  def secondTask(matrix: Matrix): Int =
    findAntenas(matrix)
      .map(producePair)
      .map { case (c, list) =>
        c -> list.flatMap(_.allAntinodes(matrix))
      }
      .flatMap { case (_, p) => p }
      .toSet
      .size

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      matrix <- readLists[IO]
      firstResult = firstTask(Matrix(matrix))
      _ <- IO.println(s"1st Task: guard visited fields: $firstResult")
      secondResult = secondTask(Matrix(matrix))
      _ <- IO.println(s"2nd Taks: guard stuck psibilities: $secondResult")
    } yield ExitCode.Success
}
