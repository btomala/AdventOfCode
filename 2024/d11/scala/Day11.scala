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
import cats.implicits.*

object Day11 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d11/input.txt"

  private def readLists[F[_]: Files: Concurrent]: F[Array[Long]] =
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.split(" ").map(_.trim.toLong))
      .compile
      .lastOrError

  def splitEvenNumber(num: Long): Option[(Long,Long)] = 
    var count = 0
    var n = num
    while (n > 0) {
      n /= 10
      count += 1
    }
    val half = Math.pow(10, count/2).toLong
    Option.when(count % 2 == 0)(num / half -> num % half)

  def firstTaskBSF(stones: Array[Long], blinks : Int): BigInt =
    def blink(stones: Vector[(Long,BigInt)], deep: Int): BigInt =
      println(s"Calculate depth $deep")
      if(deep <= 0) {
        stones.map{ case (_, multiply) => multiply}.sum[BigInt]
      } else {
        blink(
          stones.flatMap {
            case (0, multiply) => Vector(1l -> multiply)
            case (stone, multiply) => splitEvenNumber(stone) match {
              case None => Vector(stone * 2024l -> multiply)
              case Some(left, right) => Vector(left -> multiply, right -> multiply)
            }
          }.groupBy(_._1).map{
            case (stone, v) => stone -> v.map(_._2).sum
          }.toVector, deep-1)
      }
    println(s"Started with $blinks blinks")
    blink(stones.map(_ -> BigInt(1)).toVector, blinks)
    
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      stones <- readLists[IO]
      firstResult = firstTaskBSF(stones, 25)
      _ <- IO.println(s"1st Task: $firstResult")
      secondResult = firstTaskBSF(stones, 75)
      _ <- IO.println( s"2nd Taks: $secondResult" )
    } yield ExitCode.Success
}
