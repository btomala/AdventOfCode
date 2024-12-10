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

object Day9 extends IOApp {

  private given logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  private val sourceFileName = "./d9/input.txt"

  final case class MemFragment(id: Option[Int], index: Int) {
    override def toString(): String = id.map(_.toString).getOrElse(".")
  }
  final case class Mem(size: Int, id: Option[Int], index: Int) {
    override def toString(): String = (0 until size).map(_ => id.map(_.toString).getOrElse(".")).mkString
    def fragments: List[MemFragment] = (0 until size).map(i => MemFragment(id, index + i)).toList
  }

  private def readMemory[F[_]: Files: Concurrent]: F[String] =
    Files[F]
      .readAll(Path(sourceFileName))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .compile
      .toList
      .map(_.headOption.get)

  def intoMem(s: String): List[Mem] = 
    foldMap(s.zipWithIndex.toList)(0){
        case (index, (c, _)) => index + (c.toInt - '0'.toInt)
      } { 
        case (index, (c, i)) if i % 2 == 0 => Mem(c.toInt - '0'.toInt, Some(i/2), index)
        case (index, (c, i)) if i % 2 == 1 => Mem(c.toInt - '0'.toInt, None, index)
      }
  
  private def foldMap[A, B, C](list: List[A])(initial: C)(f: (C, A) => C)(transform: (C, A) => B): List[B] = {
    list.foldLeft((initial, List.empty[B])) { case ((state, acc), elem) =>
      val newState = f(state, elem)
      val transformedElem = transform(newState, elem)
      (newState, acc :+ transformedElem) // Append the transformed element to the accumulator
    }._2 // Return the accumulated transformed list
  }

  private def findLastNonEmptyFragment(mem: Array[MemFragment]): Int =
    var i = mem.size-1
    while(mem(i).id.isEmpty) {
      i = i - 1
    }
    i

  private def sortFragments(mem: Array[MemFragment]): Array[MemFragment] =
    var sortedMem = mem.clone()
    for (index <- 0 to mem.count(_.id.nonEmpty)-1) {
      val fragment = sortedMem(index)
      if (fragment.id.isEmpty) {
        val nonEmptyIndex = findLastNonEmptyFragment(sortedMem)
        val nonEmptyFragment = sortedMem(nonEmptyIndex) 
        sortedMem(nonEmptyIndex) = fragment
        sortedMem(index) = nonEmptyFragment
      }
    }
    sortedMem

  private def findLastFile(mem: Array[Mem], size: Int): Int =
    var i = mem.size-1
    while((mem(i).id.isEmpty || (mem(i).id.nonEmpty && mem(i).size > size)) && i > 0) {
      i = i - 1
    }
    i

  private def sortMem(mem: Array[Mem]): Array[MemFragment] =
    var sortedMem = mem
    var index = 0
    while (index < sortedMem.length) {
      val emptyMem = sortedMem(index)
      if (emptyMem.id.isEmpty) {
        val fileIndex = findLastFile(sortedMem, emptyMem.size)
        val file = sortedMem(fileIndex)
        if (fileIndex > index) {
          if(file.size >= emptyMem.size) {
            sortedMem(fileIndex) = emptyMem
            sortedMem(index) = file
          } else {
            val rest = emptyMem.size - file.size
            sortedMem(fileIndex) = emptyMem.copy(size = file.size)
            sortedMem(index) = file
            val (head, tail) = sortedMem.splitAt(index + 1)
            sortedMem = Array.concat(head, Array(emptyMem.copy(size = rest)),  tail)
          }
          // println(sortedMem.flatMap(_.fragments).mkString)
        }
      }
      index = index + 1
    }
    sortedMem.flatMap(_.fragments)

  private def checkSum(mem: Array[MemFragment]): Long =
    mem.zipWithIndex.foldLeft(0l){ case (agg, (fragment, index)) => fragment.id.map(_*index.toLong).getOrElse(0l) + agg}

  def firstTask(mem: List[MemFragment]): Long =
    // println(mem.mkString)
    val sorted = sortFragments(mem.toArray)
    checkSum(sorted)

  def secondTask(mem: List[Mem]): Long =
    val sorted = sortMem(mem.toArray)
    // println(sorted.mkString)
    checkSum(sorted)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger[IO].info(s"Read source data from $sourceFileName")
      mems <- readMemory[IO].map(intoMem)
      // _ <- Console[IO].print(mems.mkString)
      firstResult = firstTask(mems.flatMap(_.fragments))
      _ <- IO.println(s"1st Task: $firstResult")
      secondResult = secondTask(mems)
      _ <- IO.println(s"2nd Taks: $secondResult")
    } yield ExitCode.Success
}
