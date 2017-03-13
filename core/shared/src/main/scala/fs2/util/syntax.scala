package fs2.util

import cats.{ Traverse }

/** Provides infix syntax for the typeclasses in the util package. */
object syntax {

  /** Infix syntax for catchables. */
  implicit class CatchableOps[F[_],A](val self: F[A]) extends AnyVal {
    def attempt(implicit F: Catchable[F]): F[Attempt[A]] = F.attempt(self)
  }

  /** Infix syntax for effects. */
  implicit class EffectOps[F[_],A](val self: F[A]) extends AnyVal {
    def unsafeRunAsync(cb: Attempt[A] => Unit)(implicit F: Effect[F]): Unit = F.unsafeRunAsync(self)(cb)
  }

  /** Infix syntax for asyncs. */
  implicit class AsyncOps[F[_],A](val self: F[A]) extends AnyVal {
    def start(implicit F: Async[F]): F[F[A]] = F.start(self)
  }

  /** Infix syntax for `parallelTraverse`. */
  implicit class ParallelTraverseOps[F[_],G[_],A](val self: F[A]) extends AnyVal {
    def parallelTraverse[B](f: A => G[B])(implicit F: Traverse[F], G: Async[G]): G[F[B]] = G.parallelTraverse(self)(f)
  }

  /** Infix syntax for `parallelSequence`. */
  implicit class ParallelSequenceOps[F[_],G[_],A](val self: F[G[A]]) extends AnyVal {
    def parallelSequence(implicit F: Traverse[F], G: Async[G]): G[F[A]] = G.parallelSequence(self.asInstanceOf[F[G[A]]])
  }
}
