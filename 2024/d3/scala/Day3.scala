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

final case class State(sum: Int, filter: Boolean)
object State {
    def empty = State(0, true)
}

object Day3 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d3/input.txt"
  private val mulPattern = """mul\((\d{1,3}),(\d{1,3})\)""".r
  private val filterMulPattern = """(do\(\)|mul\(\d{1,3},\d{1,3}\)|don't\(\))""".r

  private def readLists[F[_]: Files: Concurrent]: Stream[F, String] = 
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)

  private def processCommand(state: State, command: String): State = command match {
    case "do()" => state.copy(filter = true)
    case "don't()" => state.copy(filter = false)
    case mulPattern(first, second) => if(state.filter) state.copy(sum = state.sum + first.toInt * second.toInt) else state
  }

  def firstTask[F[_]: Concurrent: Console](lol: Stream[F, String]): F[Int] =
    lol
      .map(mulPattern.findAllMatchIn)
      .flatMap(matches => Stream.emits(matches.toSeq))
    //   .evalTap(pair => Console[F].println(s"DEBUG: $pair"))
      .map(m => m.group(1).toInt -> m.group(2).toInt)
      .map(_*_)
      .compile
      .fold(0)(_ + _)

  def secondTask[F[_]: Concurrent: Console](lol: Stream[F, String]): F[Int] =    
    lol
      .map(filterMulPattern.findAllMatchIn)
      .flatMap(matches => Stream.emits(matches.toSeq))
      .map(_.group(1))
    //   .evalTap(pair => Console[F].println(s"DEBUG: $pair"))
      .scan(State.empty)(processCommand)
      .compile
      .lastOrError
      .map(_.sum)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      firstResult <- firstTask(readLists[IO])
      _ <- IO.println(s"1st Task: Uncorrupted mull result: $firstResult")
      secondResult <- secondTask(readLists[IO])
      _ <- IO.println(s"2nd Taks: Filtered Uncorrupted mull result: $secondResult")
    } yield ExitCode.Success
  }
