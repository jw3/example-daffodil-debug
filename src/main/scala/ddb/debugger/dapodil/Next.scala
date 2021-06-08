package ddb.debugger.dapodil

import cats.effect.IO
import cats.effect.kernel.Ref

/** Produce the next value in a series, like for data identifiers. */
trait Next[A] {
  def next(): IO[A]

  def map[B](f: A => B): Next[B] =
    () => next().map(f)
}

object Next {
  def int: IO[Next[Int]] =
    for {
      ids <- Ref[IO].of(0)
    } yield new Next[Int] {
      def next: IO[Int] = ids.getAndUpdate(_ + 1)
    }
}