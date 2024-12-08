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

object Day7 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d7/input.txt"

  type EQS = Vector[(Long, List[Long])]

  private def readLists[F[_]: Files: Concurrent]: F[EQS] =
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .collect(_.split(':').toList match {
        case head :: next :: Nil  => (head.toLong -> next.trim().split(" ").toList.map(_.toLong)) 
      })
      .compile
      .toVector

  def concat(x: Long, y: Long) = s"$x$y".toLong
  def concat(x: BigInt, y: BigInt) = BigInt(s"$x$y")

  def calc(agg: BigInt, ing: List[BigInt], result: BigInt): Boolean =
    ing match {
      case Nil =>
        agg == result
      case head :: Nil =>
        agg * head == result || agg + head == result || concat(agg, head) == result
      case head :: tail => 
        calc(head * agg, tail, result) || calc(head + agg, tail, result) || calc(concat(head, agg), tail, result)
    }

  def firstTask(eqs: EQS): Long =
    def calc(agg: Long, ing: List[Long], result: Long): Boolean =
      ing match {
        case Nil =>
          agg == result
        case head :: Nil =>
          agg * head == result || agg + head == result
        case head :: tail => 
          calc(head * agg, tail, result) || calc(head + agg, tail, result)
      }

    eqs.collect{
      case (result, head :: tail) => 
        if calc(head, tail, result) then Some(result) else None
    }.flatten.sum

  def secondTask(eqs: EQS): BigInt =
    def calc(agg: BigInt, ing: List[Long], result: Long): Boolean =
      ing match {
        case Nil =>
          agg == result
        case head :: Nil =>
          agg * head == result || agg + head == result || concat(agg, head) == result
        case head :: tail => 
          calc(head * agg, tail, result) || calc(head + agg, tail, result) || calc(concat(agg, head), tail, result)
      }

    eqs.collect{
      case (result, head :: tail) => 
        if calc(head, tail, result) then Some(BigInt(result)) else None
    }.flatten.sum

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      eqs <- readLists[IO]
      firstResult = firstTask(eqs)
      _ <- IO.println(s"1st Task: $firstResult")
      secondResult = secondTask(eqs)
      _ <- IO.println(s"2nd Taks: $secondResult")
    } yield ExitCode.Success
}
