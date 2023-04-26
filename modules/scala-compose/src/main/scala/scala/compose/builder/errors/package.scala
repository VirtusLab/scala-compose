package scala.compose.builder.errors

// import scala.util.boundary, boundary.{break, Label}
import scala.util.control.NonLocalReturns.*

import optional.*

// type CanError[E] = Label[Result.Failure[E]]
// type CanAbort[T, E] = Label[Result[T, E]]
type CanError[E]    = ReturnThrowable[Result.Failure[E]]
type CanAbort[T, E] = ReturnThrowable[Result[T, E]]

inline def failure[E](msg: E)(using inline canError: CanError[E]): Nothing =
  // break[Result.Failure[E]](Result.Failure(msg))
  throwReturn(Result.Failure(msg))(using canError.asInstanceOf[CanAbort[Nothing, E]])

extension [A, B, E](inline f: A => Result[B, E])
  inline def ?(using inline canError: CanError[E]): A => B = f(_).?

extension [A, B](inline f: A => Option[B])
  inline def ?(using inline canAbortNone: CanAbortNone): A => B = f(_).?

extension [T](opt: Option[T])
  inline def ?(using inline canAbortNone: CanAbortNone): T =
    opt match
      case Some(value) => value
      // case None => break(None)
      case None => throwReturn(None)
  inline def asSuccess[E](onError: => E): Result[T, E] =
    opt match
      case Some(value) => Result.Success(value)
      case None        => Result.Failure(onError)
