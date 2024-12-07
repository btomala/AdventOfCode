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

object Day6 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d6/input.txt"
  private val up = '^'
  private val obstacle = '#'
  private val visited = 'X'
  private val obstruction = 'O'

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

    def markVisited(point: Vector): Matrix =
      val newRow = aoa(point.y).clone()
      newRow.update(point.x, visited)
      val newAoA = aoa.clone()
      newAoA.update(point.y, newRow)
      Matrix(newAoA)

    def markObstructed(point: Vector): Matrix =
      val newRow = aoa(point.y).clone()
      newRow.update(point.x, obstruction)
      val newAoA = aoa.clone()
      newAoA.update(point.y, newRow)
      Matrix(newAoA)

    def setObstruction(point: Vector): Option[Matrix] =
      if (0 <= point.x && point.x < xSize && 0 <= point.y && point.y < ySize && aoa(point.y)(point.x) != obstacle) {
        Some(markObstructed(point))
      } else {
        None
      }


  final case class Vector(x: Int, y: Int):
    def +(v: Vector): Vector =
      this.copy(x + v.x, y + v.y)

  final case class Guard(position: Vector, direction: Vector):
    def visitNext(matrix: Matrix, history: Set[Guard]): (Int, Matrix) =
      val nextPosition = position + direction
      val nextGuard = Guard(nextPosition, direction)
      matrix.get(nextPosition) match {
        case Some(char) if char == obstacle || char == obstruction =>
          this.turnRight().visitNext(matrix, history)
        case Some(_) if history(nextGuard) =>
          1 -> matrix.markVisited(position)        
        case Some(_) =>
          nextGuard.visitNext(matrix.markVisited(position), history + this)
        case None =>
          0 -> matrix.markVisited(position)
      }

    private val directions =
      Array((0, -1), (1, 0), (0, 1), (-1, 0)).map(Vector.apply(_, _))

    def next(matrix: Matrix): Option[Guard] =
      val nextPosition = position + direction
      val nextGuard = Guard(nextPosition, direction)
      matrix.get(nextPosition) match {
        case Some(char) if char == obstacle || char == obstruction => Some(this.turnRight())//.next(matrix)
        case Some(char) => Some(nextGuard)
        case None => None
      }

    def turnRight(): Guard =
      val index = directions.indexOf(direction) + 1
      val newIndex = if index == directions.size then 0 else index
      this.copy(direction = directions(newIndex))

  private def findGuard(matrix: Matrix): Option[Guard] =
    matrix.aoa.zipWithIndex.flatMap { case (xArray, y) =>
      xArray.zipWithIndex.collectFirst {
        case (`up`, x)     => 
          Guard(Vector(x, y), Vector(0, -1))
      }
    }.headOption

  // 5145
  def firstTask(matrix: Matrix): Int =
    def move(g: Guard, m: Matrix): Matrix =
      g.visitNext(m, Set.empty)._2

    findGuard(matrix) match {
      case None => 0
      case Some(g) =>
        move(g, matrix).aoa.map(_.count(_ == visited)).sum
    }

  // 1628 wrong
  // 1657 too high
  def secondTask(matrix: Matrix): Int =
    def move(g: Guard, history: Set[Guard], obstructions: Set[Vector]): Set[Vector] =
      val infront = g.position + g.direction
      matrix.setObstruction(infront) match { 
        case Some(mapWithObstruction) => 
          g.visitNext(mapWithObstruction, history) match {
            case (0, _) =>
              g.next(matrix) match {
                case Some(guard) => move(guard , history + g, obstructions)
                case None => obstructions
              }
            case (1, _) =>
              // println(s"Loop detected when obstruction set at $nextPosition")
              g.next(matrix) match {
                case Some(guard) => move(guard, history + g, obstructions + infront)
                case None => obstructions
              }
          }
        case None => 
          g.next(matrix) match {
            case Some(guard) => move(guard , history + g, obstructions)
            case None => obstructions
          }
      }

    findGuard(matrix) match {
      case None => 0
      case Some(g) =>
        println(g)
        move(g, Set.empty, Set.empty).count(_ != g.position)
    }

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
