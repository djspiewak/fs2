package fs2.util

import cats.MonadError

/**
 * Monad which supports catching exceptions, suspending evaluation, and
 * (potentially asynchronous) evaluation (i.e. extraction of a value).
 *
 * For infix syntax, import `fs2.util.syntax._`.
 */
trait Effect[F[_]] extends Catchable[F] with Suspendable[F] {

  /**
   * Evaluates the specified `F[A]`, possibly asynchronously, and calls the specified
   * callback with the result of the evaluation.
   */
  def unsafeRunAsync[A](fa: F[A])(cb: Attempt[A] => Unit): Unit
}

object Effect {
  def apply[F[_]](implicit F: Effect[F]): Effect[F] = F

  implicit def monadErrorInstance[F[_]](implicit F: Effect[F]): MonadError[F, Throwable] = new MonadError[F, Throwable] {
    def pure[A](a: A) = F.pure(a)
    override def map[A, B](fa: F[A])(f: A => B) = F.map(fa)(f)
    def flatMap[A, B](fa: F[A])(f: A => F[B]) = F.flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => F[Either[A,B]]): F[B] =
      F.flatMap(f(a)) {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => pure(b)
      }
    def raiseError[A](t: Throwable) = F.fail(t)
    def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]) = F.flatMap(F.attempt(fa))(e => e.fold(f, pure))
  }
}
