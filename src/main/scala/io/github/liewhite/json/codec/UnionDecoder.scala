package io.github.liewhite.json.codec

import scala.util.NotGiven
import scala.quoted.*
import scala.compiletime.*
import io.circe.Json

trait UnionDecoder
object UnionDecoder {
  inline given union[T](using NotGiven[Decoder[T]]): Decoder[T] = ${ impl[T] }

  def impl[T: Type](using q: Quotes): Expr[Decoder[T]] = {
    import q.reflect._
    val repr = TypeRepr.of[T];
    repr match
      case OrType(a, b) =>
        (a.asType, b.asType) match
          case ('[t1], '[t2]) =>
            '{
              new Decoder[T] {
                def decode(data: Json) = {
                  val o1 = summonInline[Decoder[t1]]
                  val o2 = summonInline[Decoder[t2]]
                  // 先不用默认值进行decode，如果失败了再用默认值试一次， 避免union type 第一个type有默认值总是成功，即使应该返回第二个type
                  o1.decode(data) match {
                    case Right(o) => Right(o.asInstanceOf[T])
                    case Left(_) =>
                      o2.decode(data) match {
                        case Right(o) => Right(o.asInstanceOf[T])
                        case Left(e) => {
                            Left(e)
                        }
                      }
                  }
                }
              }
            }
      case other =>
        report.error(s"not support type:,$other"); ???

  }

}
