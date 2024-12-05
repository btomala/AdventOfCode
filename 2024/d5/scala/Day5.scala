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

object Day5 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d5/input.txt"

  private def readLists[F[_]: Files: Concurrent]
      : F[(Map[Int, Set[Int]], List[List[Int]])] =
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .split(_.isEmpty())
      .compile
      .toList
      .flatMap {
        case firstChunk :: secondChunk :: Nil =>
          for {
            orders <- parseOrder[F](Stream.chunk(firstChunk))
            pages <- parsePages[F](Stream.chunk(secondChunk))
          } yield (orders, pages)
        case _ =>
          Concurrent[F].raiseError(new Exception("Invalid input format"))
      }

  private def parsePages[F[_]: Concurrent](
      stream: Stream[F, String]
  ): F[List[List[Int]]] =
    stream.map(_.split(",").map(_.trim.toInt).toList).compile.toList

  private def parseOrder[F[_]: Concurrent](
      stream: Stream[F, String]
  ): F[Map[Int, Set[Int]]] =
    stream
      .map[List[Int]](_.split('|').map[Int](_.trim.toInt).toList)
      .flatMap {
        case before :: after :: Nil => fs2.Stream(before -> after)
        case _                      => fs2.Stream.empty[F]
      }
      .fold(Map.empty[Int, Set[Int]].withDefault(_ => Set.empty[Int])) {
        case (order, (before: Int, after: Int)) =>
          order.updated(before, order(before) + after)
      }
      .compile
      .onlyOrError

  private def identifyCorrectUpdates(orderRules: Map[Int, Set[Int]])(
      pages: List[Int]
  ): (Boolean, List[Int]) =
    pages.exists { page =>
      val afterPage = orderRules(page)
      val before = pages.take(pages.indexOf(page))
      before.exists(afterPage.contains) // if contains it's broken
    } -> pages

  def firstTask[F[_]: Applicative: Console](
      orderRules: Map[Int, Set[Int]],
      updates: List[List[Int]]
  ): F[Int] =
    Applicative[F].pure {
      updates
        .map(identifyCorrectUpdates(orderRules))
        .filterNot(_._1) // correct
        .flatMap { case (_, pages) =>
          pages.get((pages.size / 2).toInt)
        }
        .sum
    }

  def secondTask[F[_]: Applicative: Console](
      orderRules: Map[Int, Set[Int]],
      updates: List[List[Int]]
  ): F[Int] =
    Applicative[F].pure {
      updates
        .map(identifyCorrectUpdates(orderRules))
        .filter(_._1) // incorrect
        .flatMap { case (_, pages) =>
          pages
            .sortWith { case (first, second) =>
              orderRules(first).contains(second)
            }
            .get((pages.size / 2).toInt)
        }
        .sum
    }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      (orders, pages) <- readLists[IO]
      firstResult <- firstTask[IO](orders, pages)
      _ <- IO.println(s"1st Task: sum of correct middle pages is: $firstResult")
      secondResult <- secondTask[IO](orders, pages)
      _ <- IO.println(
        s"2nd Taks: sum of corrected middle pages is: $secondResult"
      )
    } yield ExitCode.Success
}
