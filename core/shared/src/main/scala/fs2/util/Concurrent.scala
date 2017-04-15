package fs2
package util

import cats.Traverse
import cats.implicits._
import cats.effect.{ Effect, IO }

import fs2.internal.{Actor,LinkedMap}
// import fs2.util.{Async,Attempt,Effect,ExecutionContexts,NonFatal}
// import ExecutionContexts._
import java.util.concurrent.atomic.{AtomicBoolean,AtomicReference}

import scala.concurrent.ExecutionContext
// import scala.concurrent.duration._

/**
 * Type class which describes effects that support asynchronous evaluation.
 *
 * Instances of this type class are defined by providing an implementation of `ref`,
 * which allocates a mutable memory cell that supports various asynchronous operations.
 *
 * For infix syntax, import `fs2.util.syntax._`.
 */
trait Concurrent[F[_]] extends Effect[F] { self =>

  /** Creates an asynchronous, concurrent mutable reference. */
  def ref[A]: F[Concurrent.Ref[F,A]]

  /** Creates an asynchronous, concurrent mutable reference, initialized to `a`. */
  def refOf[A](a: A): F[Concurrent.Ref[F,A]] = flatMap(ref[A])(r => map(r.setPure(a))(_ => r))

  /** Like `traverse` but each `F[B]` computed from an `A` is evaluated in parallel. */
  def parallelTraverse[G[_],A,B](g: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
    flatMap(G.traverse(g)(f andThen start)(self)) { gfb => G.sequence(gfb)(self) }

  /** Like `sequence` but each `F[A]` is evaluated in parallel. */
  def parallelSequence[G[_],A](v: G[F[A]])(implicit G: Traverse[G]): F[G[A]] =
    parallelTraverse(v)(identity)

  /**
   * Begins asynchronous evaluation of `f` when the returned `F[F[A]]` is
   * bound. The inner `F[A]` will block until the result is available.
   */
  def start[A](f: F[A]): F[F[A]] =
    flatMap(ref[A]) { ref =>
    flatMap(ref.set(f)) { _ => pure(ref.get) }}
}

object Concurrent {
  def apply[F[_]](implicit F: Concurrent[F]): Concurrent[F] = F

  /** Creates an asynchronous, concurrent mutable reference. */
  def ref[F[_],A](implicit F: Concurrent[F]): F[Concurrent.Ref[F,A]] = F.ref

  /** Creates an asynchronous, concurrent mutable reference, initialized to `a`. */
  def refOf[F[_],A](a: A)(implicit F: Concurrent[F]): F[Concurrent.Ref[F,A]] = F.refOf(a)

  /** An asynchronous, concurrent mutable reference. */
  trait Ref[F[_],A] { self =>
    implicit protected val F: Concurrent[F]

    /**
     * Obtains a snapshot of the current value of the `Ref`, and a setter
     * for updating the value. The setter may noop (in which case `false`
     * is returned) if another concurrent call to `access` uses its
     * setter first. Once it has noop'd or been used once, a setter
     * never succeeds again.
     */
    def access: F[(A, Attempt[A] => F[Boolean])]

    /** Obtains the value of the `Ref`, or wait until it has been `set`. */
    def get: F[A] = access.map(_._1)

    /** Like `get`, but returns an `F[Unit]` that can be used cancel the subscription. */
    def cancellableGet: F[(F[A], F[Unit])]

    /**
     * Tries modifying the reference once, returning `None` if another
     * concurrent `set` or `modify` completes between the time
     * the variable is read and the time it is set.
     */
    def tryModify(f: A => A): F[Option[Change[A]]] =
      access.flatMap { case (previous,set) =>
        val now = f(previous)
        set(Right(now)).map { b =>
          if (b) Some(Change(previous, now))
          else None
        }
      }

    /** Like `tryModify` but allows to return `B` along with change. **/
    def tryModify2[B](f: A => (A,B)): F[Option[(Change[A], B)]] =
      access.flatMap { case (previous,set) =>
        val (now,b0) = f(previous)
        set(Right(now)).map { b =>
          if (b) Some(Change(previous, now) -> b0)
          else None
        }
    }

    /** Repeatedly invokes `[[tryModify]](f)` until it succeeds. */
    def modify(f: A => A): F[Change[A]] =
      tryModify(f).flatMap {
        case None => modify(f)
        case Some(change) => F.pure(change)
      }

    /** Like modify, but allows to extra `b` in single step. **/
    def modify2[B](f: A => (A,B)): F[(Change[A], B)] =
      tryModify2(f).flatMap {
        case None => modify2(f)
        case Some(changeAndB) => F.pure(changeAndB)
      }

    /**
     * *Asynchronously* sets a reference. After the returned `F[Unit]` is bound,
     * the task is running in the background. Multiple tasks may be added to a
     * `Ref[A]`.
     *
     * Satisfies: `r.set(t) flatMap { _ => r.get } == t`.
     */
    def set(a: F[A]): F[Unit]

    /**
     * *Asynchronously* sets a reference to a pure value.
     *
     * Satisfies: `r.setPure(a) flatMap { _ => r.get(a) } == pure(a)`.
     */
    def setPure(a: A): F[Unit] = set(F.pure(a))
  }

  /**
   * The result of a `Ref` modification. `previous` is the value before modification
   * (the value passed to modify function, `f` in the call to `modify(f)`. `now`
   * is the new value computed by `f`.
   */
  final case class Change[+A](previous: A, now: A) {
    def modified: Boolean = previous != now
    def map[B](f: A => B): Change[B] = Change(f(previous), f(now))
  }

  def mk[F[_]](F: Effect[F])(implicit ec: ExecutionContext): Concurrent[F] = {
    trait MsgId
    trait Msg[A]
    object Msg {
      final case class Read[A](cb: Attempt[(A, Long)] => Unit, id: MsgId) extends Msg[A]
      final case class Nevermind[A](id: MsgId, cb: Attempt[Boolean] => Unit) extends Msg[A]
      final case class Set[A](r: Attempt[A]) extends Msg[A]
      final case class TrySet[A](id: Long, r: Attempt[A], cb: Attempt[Boolean] => Unit) extends Msg[A]
    }

    new Concurrent[F] { self =>
      def pure[A](a: A): F[A] = F.pure(a)
      def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)
      def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] = F.async(k)
      def runAsync[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] = F.runAsync(fa)(cb)
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
      def liftIO[A](ioa: IO[A]): F[A] = F.liftIO(ioa)
      def suspend[A](thunk: => F[A]): F[A] = F.suspend(thunk)
      def ref[A]: F[Concurrent.Ref[F,A]] = F.delay {
        var result: Attempt[A] = null
        // any waiting calls to `access` before first `set`
        var waiting: LinkedMap[MsgId, Attempt[(A, Long)] => Unit] = LinkedMap.empty
        // id which increases with each `set` or successful `modify`
        var nonce: Long = 0

        val actor: Actor[Msg[A]] = Actor.actor[Msg[A]] {
          case Msg.Read(cb, idf) =>
            if (result eq null) {
              waiting = waiting.updated(idf, cb)
            } else {
              val r = result
              val id = nonce
              F.runAsync(F.delay(cb(r.map((_,id)))))(_ => IO.pure(())).unsafeRunSync
            }

          case Msg.Set(r) =>
            nonce += 1L
            if (result eq null) {
              val id = nonce
              waiting.values.foreach { cb =>
                F.runAsync(F.delay(cb(r.map((_,id)))))(_ => IO.pure(())).unsafeRunSync
              }
              waiting = LinkedMap.empty
            }
            result = r

          case Msg.TrySet(id, r, cb) =>
            if (id == nonce) {
              nonce += 1L; val id2 = nonce
              waiting.values.foreach { cb =>
                F.runAsync(F.delay(cb(r.map((_,id2)))))(_ => IO.pure(())).unsafeRunSync
              }
              waiting = LinkedMap.empty
              result = r
              cb(Right(true))
            }
            else cb(Right(false))

          case Msg.Nevermind(id, cb) =>
            val interrupted = waiting.get(id).isDefined
            waiting = waiting - id
            F.runAsync(F.delay(cb(Right(interrupted))))(_ => IO.pure(())).unsafeRunSync
        }

        new Ref[F, A] {
          implicit val F: Concurrent[F] = self

          def access: F[(A, Attempt[A] => F[Boolean])] =
            F.delay(new MsgId {}).flatMap { mid =>
              getStamped(mid).map { case (a, id) =>
                val set = (a: Attempt[A]) =>
                  F.async[Boolean] { cb => actor ! Msg.TrySet(id, a, cb) }
                (a, set)
              }
            }

          /**
           * Return a `F` that submits `t` to this ref for evaluation.
           * When it completes it overwrites any previously `put` value.
           */
          def set(t: F[A]): F[Unit] =
            F.delay(F.runAsync(t)(r => IO(actor ! Msg.Set(r))).unsafeRunSync)

          private def getStamped(msg: MsgId): F[(A,Long)] =
            F.async[(A,Long)] { cb => actor ! Msg.Read(cb, msg) }

          /** Return the most recently completed `set`, or block until a `set` value is available. */
          override def get: F[A] = F.delay(new MsgId {}).flatMap { mid => getStamped(mid).map(_._1) }

          /** Like `get`, but returns a `F[Unit]` that can be used cancel the subscription. */
          def cancellableGet: F[(F[A], F[Unit])] = F.delay {
            val id = new MsgId {}
            val get = getStamped(id).map(_._1)
            val cancel = F.async[Unit] {
              cb => actor ! Msg.Nevermind(id, r => cb(r.map(_ => ())))
            }
            (get, cancel)
          }

          /**
           * Runs `t1` and `t2` simultaneously, but only the winner gets to
           * `set` to this `ref`. The loser continues running but its reference
           * to this ref is severed, allowing this ref to be garbage collected
           * if it is no longer referenced by anyone other than the loser.
           */
          def setRace(t1: F[A], t2: F[A]): F[Unit] = F.delay {
            val ref = new AtomicReference(actor)
            val won = new AtomicBoolean(false)
            val win = (res: Attempt[A]) => {
              // important for GC: we don't reference this ref
              // or the actor directly, and the winner destroys any
              // references behind it!
              if (won.compareAndSet(false, true)) {
                val actor = ref.get
                ref.set(null)
                actor ! Msg.Set(res)
              }
            }
            F.runAsync(t1)(res => IO(win(res))).unsafeRunSync
            F.runAsync(t2)(res => IO(win(res))).unsafeRunSync
          }
        }
      }
    }
  }

  implicit def ioInstance(implicit ec: ExecutionContext): Concurrent[IO] = mk(IO.ioEffect)(ec)
}
