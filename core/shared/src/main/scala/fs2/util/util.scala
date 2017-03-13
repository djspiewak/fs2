package fs2

/**
 * Type classes and other utilities used by [[fs2]].
 *
 * == Type Classes ==
 *
 * This package includes a set of common functional type classes related to effect capture --
 * [[Suspendable]], [[Catchable]], [[Effect]], and [[Async]].
 *
 * Infix syntax is provided for these type classes by the [[fs2.util.syntax]] object, which is used by
 * adding `import fs2.util.syntax._`. The infix syntax has no runtime cost.
 */
package object util {

  /** Operator alias for `UF1[F,G]`. */
  type ~>[-F[_],+G[_]] = UF1[F,G]

  /** Alias for `Either[Throwable,A]`. */
  type Attempt[+A] = Either[Throwable,A]
}
