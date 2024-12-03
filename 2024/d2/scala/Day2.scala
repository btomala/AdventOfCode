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

object Day2 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d2/input.txt"

  private def readLists[F[_]: Files: Concurrent]: F[List[List[Int]]] = 
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.split(" ").toList.map(_.toInt))
      .compile
      .toList

  private def calculateSafeLevels(list: List[Int]): List[Boolean] =
    val tuples = list.sliding2
    val maybeIncreasing = tuples.headOption.flatMap{
      case (first, second) if second - first == 0 => None
      case (first, second) => Some(second - first > 0)
    }
    tuples.map {
      case (previous, next) => 
        val diff = next - previous
        val absDiff = Math.abs(diff)
        absDiff >= 1 && absDiff <= 3 && maybeIncreasing.forall(_ == diff > 0) && maybeIncreasing.nonEmpty    
    }

  def firstTask(lol: List[List[Int]]): IO[Int] =
    IO.apply(
      lol.count(calculateSafeLevels(_).forall(identity))
    )

  def secondTask(lol: List[List[Int]]): IO[Int] =    
    IO.apply(
      lol.count{ list =>
        val safeLevels = calculateSafeLevels(list)
        if (safeLevels.count(_ == false) > 0) {
          list.zipWithIndex.map{ case (_, i) => list.take(i) ++ list.drop(i + 1)}
          .map(newlist => calculateSafeLevels(newlist).forall(identity)).exists(identity)
        } else safeLevels.forall(identity)
      }
    )

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      tList <- readLists[IO]
      firstResult <- firstTask(tList)
      _ <- IO.println(s"1st Task: Number of safe reports: $firstResult")
      secondResult <- secondTask(tList)
      _ <- IO.println(s"2nd Taks: Safe reports with single bad level: $secondResult")
    } yield ExitCode.Success
  }
