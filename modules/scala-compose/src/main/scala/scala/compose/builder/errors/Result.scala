package scala.compose.builder.errors

// import scala.util.boundary, boundary.break
import scala.util.control.NonLocalReturns.*

enum Result[+T, +E]:
  case Success[+T](value: T) extends Result[T, Nothing]
  case Failure[+E](error: E) extends Result[Nothing, E]

  // inline def ?(using inline canError: CanError[E]): T = this match
  inline def ?[E1 >: E](using inline canError: CanError[E1]): T = this match
    case Success(value) => value
    // case err: Failure[E] => break(err)
    case err: Failure[E] => throwReturn(err)

object Result:
  // inline def apply[T, E](inline body: CanError[E] ?=> T): Result[T, E] =
  inline def apply[T, E](inline body: CanError[E] ?=> T): Result[T, E] =
    // boundary:
    //   Result.Success(body)
    returning { ce ?=>
      Result.Success(body(using ce.asInstanceOf[CanError[E]]))
    }

  inline def attempt[T](inline body: => T): Result[T, Exception] =
    try Result.Success(body)
    catch case e: Exception => Result.Failure(e)

  extension [T, E <: Exception](result: Result[T, E])
    inline def resolve[E1](inline onError: E => E1): Result[T, E1] = result match
      case succ @ Success(value) => succ
      case Failure(error) => Failure(onError(error))