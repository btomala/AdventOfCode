package bt.aoc2024.d1

import cats.syntax.all.*
import cats.effect.*
import cats.effect.std.Console
import cats.effect.kernel.Sync
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.*
import fs2.io.file.{Files, Path}

object Day1 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d1/input.txt"

  private def readLists[F[_]: Files: Concurrent]: F[List[(Int, Int)]] = 
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.split("   ").toList)
      .collect{
        case left :: right :: Nil => (left.toInt, right.toInt)
      }
      .compile
      .toList

  def firstTask(tList: List[(Int, Int)]): IO[Int] =    
    val lList = tList.map(_._1).sorted
    val rList = tList.map(_._2).sorted
    val dList = lList.zip(rList).map(_ - _).map(Math.abs)
    IO.apply(dList.sum)

  def secondTask(tList: List[(Int, Int)]): IO[Int] =    
    val lList = tList.map(_._1).sorted
    val rList = tList.map(_._2).sorted
    val sList = lList.map(d => d * rList.count(_ == d))
    IO.apply(sList.sum)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      tList <- readLists[IO]
      firstResult <- firstTask(tList)
      _ <- IO.println(s"1st Task: Total distance is: $firstResult")
      secondResult <- secondTask(tList)
      _ <- IO.println(s"2nd Taks: Similarity score is: $secondResult")
    } yield ExitCode.Success
  }
