package io.github.liewhite.json.codec

import scala.quoted.*
import scala.compiletime.*

trait UnionEncoder {}

object UnionEncoder {
  inline given union[T]: Encoder[T] = {
    ${ impl[T] }
  }

  def impl[T: Type](using q: Quotes): Expr[Encoder[T]] =
    import q.reflect._

    val repr = TypeRepr.of[T];
    repr match
      case OrType(a, b) =>
        (a.asType, b.asType) match
          case ('[t1], '[t2]) =>
            '{
              new Encoder[T] {
                def encode(t: T) =
                  val o1 = summonInline[Encoder[t1]]
                  val o2 = summonInline[Encoder[t2]]
                  t match
                    case o: t1 => o1.encode(o)
                    case o: t2 => o2.encode(o)
              }
            }
      case other =>
        report.error(s"not support type:,$other"); ???

}
