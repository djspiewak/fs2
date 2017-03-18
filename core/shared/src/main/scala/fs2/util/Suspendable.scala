package fs2.util

import cats.Monad
import cats.data.Kleisli

/**
 * Monad which supports capturing a deferred evaluation of a by-name `F[A]`.
 *
 * Evaluation is suspended until a value is extracted, typically via the `unsafeRunAsync`
 * method on the related [[Effect]] type class or via a type constructor specific extraction
 * method (e.g., `unsafeRunSync` on `Task`). Side-effects that occur while evaluating a
 * suspension are evaluated exactly once at the time of extraction.
 */
trait Suspendable[F[_]] extends Monad[F] {

  /**
   * Returns an `F[A]` that evaluates and runs the provided `fa` on each run.
   */
  def suspend[A](fa: => F[A]): F[A]

  /**
   * Promotes a non-strict value to an `F`.
   * Evaluates `a` each time the returned effect is run.
   */
  def delay[A](a: => A): F[A] = suspend(pure(a))
}

object Suspendable {
  def apply[F[_]](implicit F: Suspendable[F]): Suspendable[F] = F

  implicit def kleisliSuspendableInstance[F[_], E](implicit F: Suspendable[F]): Suspendable[Kleisli[F, E, ?]] = new Suspendable[Kleisli[F, E, ?]] {
    def pure[A](a: A): Kleisli[F, E, A] = Kleisli.pure[F, E, A](a)
    override def map[A, B](fa: Kleisli[F, E, A])(f: A => B): Kleisli[F, E, B] = fa.map(f)
    def flatMap[A, B](fa: Kleisli[F, E, A])(f: A => Kleisli[F, E, B]): Kleisli[F, E, B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Kleisli[F, E, Either[A, B]]): Kleisli[F, E, B] = defaultTailRecM[Kleisli[F, E, ?], A, B](a)(f)
    def suspend[A](fa: => Kleisli[F, E, A]): Kleisli[F, E, A] = Kleisli(e => F.suspend(fa.run(e)))
  }
}
