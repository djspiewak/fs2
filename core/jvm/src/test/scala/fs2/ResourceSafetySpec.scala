package fs2

import java.util.concurrent.atomic.AtomicLong
import cats.effect.IO
import org.scalacheck._
import fs2.util.Concurrent

class ResourceSafetySpec extends Fs2Spec with EventuallySupport {

  "Resource Safety" - {

    "pure fail" in {
      an[Err.type] should be thrownBy {
        Stream.emit(0).flatMap(_ => Stream.fail(Err)).toVector
        ()
      }
    }

    "bracket (normal termination / failing)" in forAll { (s0: List[PureStream[Int]], f: Failure, ignoreFailure: Boolean) =>
      val c = new AtomicLong(0)
      val s = s0.map { s => bracket(c)(if (ignoreFailure) s.get else spuriousFail(s.get,f)) }
      val s2 = s.foldLeft(Stream.empty: Stream[IO,Int])(_ ++ _)
      swallow { runLog(s2) }
      swallow { runLog(s2.take(1000)) }
      withClue(f.tag) { 0L shouldBe c.get }
    }

    "bracket ++ throw Err" in forAll { (s: PureStream[Int]) =>
      val c = new AtomicLong(0)
      val b = bracket(c)(s.get ++ ((throw Err): Stream[Pure,Int]))
      swallow { b }
      c.get shouldBe 0
    }

    "1 million brackets in sequence" in {
      val c = new AtomicLong(0)
      val b = bracket(c)(Stream.emit(1))
      val bs = Stream.range(0, 1000000).flatMap(_ => b)
      runLog { bs }
      c.get shouldBe 0
    }

    "nested bracket" in forAll { (s0: List[Int], f: Failure, finalizerFail: Boolean) =>
      val c = new AtomicLong(0)
      // construct a deeply nested bracket stream in which the innermost stream fails
      // and check that as we unwind the stack, all resources get released
      // Also test for case where finalizer itself throws an error
      val innermost =
        if (!finalizerFail) f.get
        else Stream.bracket(IO(c.decrementAndGet))(
               _ => f.get,
               _ => IO { c.incrementAndGet; throw Err })
      val nested = Chunk.seq(s0).foldRight(innermost)((i,inner) => bracket(c)(Stream.emit(i) ++ inner))
      try { runLog { nested }; throw Err } // this test should always fail, so the `run` should throw
      catch { case Err => () }
      withClue(f.tag) { 0L shouldBe c.get }
    }

    case class Small(get: Long)
    implicit def smallLongArb = Arbitrary(Gen.choose(0L,10L).map(Small(_)))

    "early termination of bracket" in forAll { (s: PureStream[Int], i: Small, j: Small, k: Small) =>
      val c = new AtomicLong(0)
      val bracketed = bracket(c)(s.get)
      runLog { bracketed.take(i.get) } // early termination
      runLog { bracketed.take(i.get).take(j.get) } // 2 levels termination
      runLog { bracketed.take(i.get).take(i.get) } // 2 levels of early termination (2)
      runLog { bracketed.take(i.get).take(j.get).take(k.get) } // 3 levels of early termination
      // Let's just go crazy
      runLog { bracketed.take(i.get).take(j.get).take(i.get).take(k.get).take(j.get).take(i.get) }
      withClue(s.tag) { 0L shouldBe c.get }
    }

    "early termination of uncons" in {
      var n = 0
      Stream(1,2,3).onFinalize(IO(n = 1)).uncons.run.unsafeRunSync
      n shouldBe 1
    }

    "bracket release should not be called until necessary" in {
      val buffer = collection.mutable.ListBuffer[Symbol]()
      runLog {
        val s = Stream.bracket(IO(buffer += 'Acquired))(
          _ => {
            buffer += 'Used
            Stream.emit(())
          },
          _ => {
            buffer += 'ReleaseInvoked
            IO { buffer += 'Released; () }
          })
        s.flatMap { s => buffer += 'FlatMapped; Stream.emit(s) }
      }
      buffer.toList shouldBe List('Acquired, 'Used, 'FlatMapped, 'ReleaseInvoked, 'Released)
    }

    "asynchronous resource allocation (1)" in forAll { (s1: PureStream[Int], f1: Failure) =>
      val c = new AtomicLong(0)
      val b1 = bracket(c)(s1.get)
      val b2 = f1.get
      swallow { runLog { b1.merge(b2) }}
      swallow { runLog { b2.merge(b1) }}
      eventually { c.get shouldBe 0L }
    }

    "asynchronous resource allocation (2a)" in forAll { (u: Unit) =>
      val s1 = Stream.fail(Err)
      val s2 = Stream.fail(Err)
      val c = new AtomicLong(0)
      val b1 = bracket(c)(s1)
      val b2 = s2: Stream[IO,Int]
      // subtle test, get different scenarios depending on interleaving:
      // `s2` completes with failure before the resource is acquired by `s2`.
      // `b1` has just caught `s1` error when `s2` fails
      // `b1` fully completes before `s2` fails
      swallow { runLog { b1 merge b2 }}
      eventually { c.get shouldBe 0L }
    }

    "asynchronous resource allocation (2b)" in forAll { (s1: PureStream[Int], s2: PureStream[Int], f1: Failure, f2: Failure) =>
      val c = new AtomicLong(0)
      val b1 = bracket(c)(s1.get)
      val b2 = bracket(c)(s2.get)
      swallow { runLog { spuriousFail(b1,f1) merge b2 }}
      swallow { runLog { b1 merge spuriousFail(b2,f2) }}
      swallow { runLog { spuriousFail(b1,f1) merge spuriousFail(b2,f2) }}
      eventually { c.get shouldBe 0L }
    }

    "asynchronous resource allocation (3)" in forAll {
      (s: PureStream[(PureStream[Int], Option[Failure])], allowFailure: Boolean, f: Failure, n: SmallPositive) =>
      val outer = new AtomicLong(0)
      val inner = new AtomicLong(0)
      val s2 = bracket(outer)(s.get.map { case (s, f) =>
        if (allowFailure) f.map(f => spuriousFail(bracket(inner)(s.get), f)).getOrElse(bracket(inner)(s.get))
        else bracket(inner)(s.get)
      })
      swallow { runLog { concurrent.join(n.get)(s2).take(10) }}
      swallow { runLog { concurrent.join(n.get)(s2) }}
      outer.get shouldBe 0L
      eventually { inner.get shouldBe 0L }
    }

    "asynchronous resource allocation (4)" in forAll { (s: PureStream[Int], f: Option[Failure]) =>
      val c = new AtomicLong(0)
      val s2 = bracket(c)(f match {
        case None => s.get
        case Some(f) => spuriousFail(s.get, f)
      })
      swallow { runLog { s2 through pipe.prefetch }}
      swallow { runLog { s2 through pipe.prefetch through pipe.prefetch }}
      swallow { runLog { s2 through pipe.prefetch through pipe.prefetch through pipe.prefetch }}
      eventually { c.get shouldBe 0L }
    }

    "asynchronous resource allocation (5)" in forAll { (s: PureStream[PureStream[Int]]) =>
      val signal = async.signalOf[IO,Boolean](false).unsafeRunSync()
      val c = new AtomicLong(0)
      IO { Thread.sleep(20L) }.flatMap(_ => signal.set(true)).shift.unsafeRunSync()
      runLog { s.get.evalMap { inner =>
        Concurrent[IO].start(bracket(c)(inner.get).evalMap { _ => IO.async[Unit](_ => ()) }.interruptWhen(signal.continuous).run)
      }}
      eventually { c.get shouldBe 0L }
    }

    "asynchronous resource allocation (6)" in {
      // simpler version of (5) above which previously failed reliably, checks the case where a
      // stream is interrupted while in the middle of a resource acquire that is immediately followed
      // by a step that never completes!
      val s = Stream(Stream(1))
      val signal = async.signalOf[IO,Boolean](false).unsafeRunSync()
      val c = new AtomicLong(1)
      IO { Thread.sleep(20L) }.flatMap(_ => signal.set(true)).shift.unsafeRunSync() // after 20 ms, interrupt
      runLog { s.evalMap { inner => Concurrent[IO].start {
        Stream.bracket(IO { Thread.sleep(2000) })( // which will be in the middle of acquiring the resource
          _ => inner,
          _ => IO { c.decrementAndGet; () }
        ).evalMap { _ => IO.async[Unit](_ => ()) }.interruptWhen(signal.discrete).run
      }}}
      eventually { c.get shouldBe 0L }
    }

    "evaluating a bracketed stream multiple times is safe" in {
      val s = Stream.bracket(IO.pure(()))(Stream.emit, _ => IO.pure(())).run
      s.unsafeRunSync
      s.unsafeRunSync
    }

    def bracket[A](c: AtomicLong)(s: Stream[IO,A]): Stream[IO,A] = Stream.suspend {
      Stream.bracket(IO { c.decrementAndGet })(
        _ => s,
        _ => IO { c.incrementAndGet; () })
    }
  }
}
