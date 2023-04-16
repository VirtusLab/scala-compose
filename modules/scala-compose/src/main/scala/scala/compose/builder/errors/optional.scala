package scala.compose.builder.errors

// import scala.util.boundary, boundary.break, boundary.Label
import scala.util.control.NonLocalReturns.*

object optional:

  // type CanAbortNone = Label[None.type]
  type CanAbortNone = ReturnThrowable[None.type]

  inline def apply[T](inline body: CanAbortNone ?=> T): Option[T] =
    // boundary:
    //   Some(body)
    returning[Option[T]] { ca ?=>
      Some(body(using ca.asInstanceOf[CanAbortNone]))
    }
