package fs2
package util

import cats.{ Monad, MonadError }
import cats.data.Kleisli

/**
 * Monad which tracks exceptions thrown during evaluation.
 *
 * For infix syntax, import `fs2.util.syntax._`.
 */
trait Catchable[F[_]] extends Monad[F] {
  /** Lifts a pure exception in to the error mode of the `F` effect. */
  def fail[A](err: Throwable): F[A]
  /** Provides access to the error in `fa`, if present, by wrapping it in a `Left`. */
  def attempt[A](fa: F[A]): F[Attempt[A]]
}

object Catchable {
  def apply[F[_]](implicit F: Catchable[F]): Catchable[F] = F

  implicit val attemptInstance: Catchable[Attempt] = new Catchable[Attempt] {
    def pure[A](a: A): Attempt[A] = Right(a)
    def flatMap[A, B](a: Attempt[A])(f: A => Attempt[B]): Attempt[B] = a.flatMap(f)
    def tailRecM[B, C](b: B)(f: B => Either[Throwable, Either[B, C]]): Either[Throwable, C] =
      cats.instances.either.catsStdInstancesForEither.tailRecM(b)(f)
    def attempt[A](a: Attempt[A]): Attempt[Attempt[A]] = a match {
      case Right(a) => Right(Right(a))
      case Left(t) => Right(Left(t))
    }
    def fail[A](t: Throwable): Attempt[A] = Left(t)
    override def toString = "Catchable[Attempt]"
  }

  implicit def monadErrorInstance[F[_]](implicit F: Catchable[F]): MonadError[F, Throwable] = new MonadError[F, Throwable] {
    def pure[A](a: A) = F.pure(a)
    override def map[A, B](fa: F[A])(f: A => B) = F.map(fa)(f)
    def flatMap[A, B](fa: F[A])(f: A => F[B]) = F.flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => F[Either[A,B]]): F[B] = defaultTailRecM(a)(f)(F)
    def raiseError[A](t: Throwable) = F.fail(t)
    def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]) = F.flatMap(F.attempt(fa))(e => e.fold(f, pure))
  }

  implicit def kleisliCatchableInstance[F[_], E](implicit F: Catchable[F]): Catchable[Kleisli[F, E, ?]] = new Catchable[Kleisli[F, E, ?]] {
    def pure[A](a: A): Kleisli[F, E, A] = Kleisli.pure[F, E, A](a)(F)
    override def map[A, B](fa: Kleisli[F, E, A])(f: A => B): Kleisli[F, E, B] = fa.map(f)(F)
    def flatMap[A, B](fa: Kleisli[F, E, A])(f: A => Kleisli[F, E, B]): Kleisli[F, E, B] = fa.flatMap(f)(F)
    def tailRecM[A, B](a: A)(f: A => Kleisli[F, E, Either[A, B]]): Kleisli[F, E, B] = defaultTailRecM[Kleisli[F, E, ?], A, B](a)(f)
    def attempt[A](fa: Kleisli[F, E, A]): Kleisli[F, E, Attempt[A]] = Kleisli(e => F.attempt(fa.run(e)))
    def fail[A](t: Throwable): Kleisli[F, E, A] = Kleisli(e => F.fail(t))
  }
}
