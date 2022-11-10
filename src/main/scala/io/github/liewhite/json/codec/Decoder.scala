package io.github.liewhite.json.codec

import scala.deriving.*
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.util.NotGiven
import scala.compiletime.*
import java.math.BigInteger
import scala.reflect.ClassTag
import shapeless3.deriving.*
import io.github.liewhite.json.annotations.*
import io.github.liewhite.json.typeclass.*

import io.circe.Json
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime
import scala.util.Try
import scala.util.Failure.apply
import cats.syntax.validated
import scala.util.Failure

trait Decoder[T] extends ProductDecoder {
  def decode(
      data: Json
  ): Either[DecodeException, T]
}

// 对于Enum 的case， SumOf和ProductOf都可以满足， 不通过继承区分优先级的话，就会出错
// fixed in scala v3.1.3
trait CoproductDecoder extends UnionDecoder

object CoproductDecoder {
  given coproduct[T](using
      inst: => K0.CoproductInstances[Decoder, T],
      labelling: Labelling[T]
  ): Decoder[T] = {
    new Decoder[T] {
      def decode(
          data: Json,
          
      ): Either[DecodeException, T] = {
        def decodePhase(
            inst: K0.CoproductInstances[Decoder, T],
            labelling: Labelling[T],
            data: Json
        ): Option[T] = {
          val result = labelling.elemLabels.zipWithIndex.iterator
            .map((p: (String, Int)) => {
              val i = p._2
              inst.project[Json](i)(data)(
                [t] =>
                  (s: Json, rt: Decoder[t]) => (data, rt.decode(s).toOption)
              ) match
                case (s, None)     => None
                case (tl, Some(t)) => Some(t)
            })
            .find(_.isDefined)
            .flatten
          result
        }

        if (data.isString) {
          val ordinal = labelling.elemLabels.indexOf(data.asString.get)
          inst.project[Json](ordinal)(data)(
            [t] => (s: Json, rt: Decoder[t]) => (s, rt.decode(data).toOption)
          ) match
            case (s, None) =>
              Left(
                DecodeException(
                  s"cant decode to :${labelling.label}\n" + data.spaces2
                )
              )
            case (tl, Some(t)) => Right(t)
        } else {
          // 可能排在前面的case每个field都有默认值， 就匹配不到真正符合条件的case了
          val result = decodePhase(inst, labelling, data)

          result match {
            case Some(v) => Right(v)
            case None =>
              Left(DecodeException("can't decode :" + labelling.label))
          }
        }
      }
    }
  }
}

trait ProductDecoder extends CoproductDecoder
object ProductDecoder {
  given product[T](using
      inst: => K0.ProductInstances[Decoder, T],
      labelling: Labelling[T],
      defaults: DefaultValue[T],
      objAnn: RepeatableAnnotation[ObjDecodeAnnotation, T],
      fieldAnns: RepeatableAnnotations[FieldDecodeAnnotation, T]
  ): Decoder[T] =
    new Decoder[T]:
      def decode(
          data: Json,
          
      ): Either[DecodeException, T] =
        def decodePhase[T](
            inst: K0.ProductInstances[Decoder, T],
            data: Json,
            labelling: Labelling[T],
            defaultValues: Map[String, Json]
        ) = {
          val label = labelling.label
          val fieldsName = labelling.elemLabels
          val itemsData: Json =
            if data.isString then
              val stringValue = data.asString.get
              // 如果是字符串， 那么可能是 遇到 没有参数的 Enum了
              if stringValue == label then
                Json.fromFields(
                  Map.empty[String, Json]
                )
              else throw new DecodeException("label not equals enum name")
            else if data.isObject then data
            else
              throw new DecodeException(
                s"expect product, got: ${data.toString}"
              )

          var index = 0
          val afterFieldsAnns = fieldsName
            .zip(fieldAnns())
            .foldLeft(itemsData)((acc, item) => {
              item._2.foldLeft(acc)((iResult, i) => {
                i.beforeDecode(item._1, iResult)
              })
            })
          val afterObjAnns = objAnn().foldLeft(afterFieldsAnns)((acc, item) => {
            item.beforeDecode(acc)
          })
          val result = inst.construct(
            [t] =>
              (itemDecoder: Decoder[t]) => {
                // val jsonData = afterFieldsAnns.asObject.get.apply(fieldsName(index))
                val jsonData =
                  afterObjAnns.asObject.get.apply(fieldsName(index))
                // 处理默认值
                val value = jsonData match {
                  case None =>
                    if (defaultValues.contains(fieldsName(index))) {
                      defaultValues(fieldsName(index))
                    } else {
                      Json.Null
                    }
                  case Some(v) => v
                }
                val item = itemDecoder.decode(value) match {
                  case Right(o) => o
                  case Left(e)  => throw e
                }
                index += 1
                item
            }
          )
          Right(result)
        }
        try {
          decodePhase[T](
            inst,
            data,
            labelling,
            defaults.defaults
          )
        } catch {
          case e: DecodeException => Left(e)
        }

}

object Decoder:
  def decodeError(expect: String, got: Json) = Left(
    DecodeException(s"expect $expect, but ${got.toString} found")
  )
  inline def derived[T](using
      gen: K0.Generic[T],
      labelling: Labelling[T]
  ): Decoder[T] =
    gen.derive(ProductDecoder.product, CoproductDecoder.coproduct)

  def decodeSeq[T](data: Json)(using
      innerDecoder: Decoder[T]
  ): Either[DecodeException, List[T]] =
    if data.isArray then
      val array = data
      val decodedArray = array.asArray.get.map(innerDecoder.decode(_))
      val failed = decodedArray.find(_.isLeft)
      failed match
        case Some(failItem) => Left(failItem.left.toOption.get)
        case None           => Right(decodedArray.map(_.toOption.get).toList)
    else decodeError("Json.JArray", data)

  given [T](using innerDecoder: Decoder[T]): Decoder[Map[String, T]] with
    def decode(
        data: Json
    ): Either[DecodeException, Map[String, T]] =
      if data.isObject then
        val decodedArray = data.asObject.get.toMap.map { case (k, v) =>
          (k, innerDecoder.decode(v))
        }
        val failed = decodedArray.find(_._2.isLeft)
        failed match
          case Some(failItem) => Left(failItem._2.left.toOption.get)
          case None =>
            Right(decodedArray.map { case (k, v) => (k, v.toOption.get) }.toMap)
      else decodeError("Json.JObject", data)

  given Decoder[EmptyTuple] with
    def decode(data: Json) = Right(EmptyTuple)

  given [H, T <: Tuple](using
      headEncoder: => Decoder[H],
      tailEncoder: => Decoder[T]
  ): Decoder[H *: T] with
    def decode(data: Json) = {
      data.asArray match {
        case Some(a) => {
          headEncoder.decode(a.head) match {
            case Right(rh) => {
              tailEncoder.decode(Json.fromValues(a.tail)) match {
                case Right(rt) => Right(rh *: rt)
                case Left(l)   => Left(l)
              }
            }
            case Left(l) => Left(l)
          }
        }
        case None => decodeError("array", data)
      }
    }

  /** unit decoder
    */
  given Decoder[Unit] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Unit] = {
      Right(())
    }

  given Decoder[Json] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Json] = {
      Right(data)

    }

  given [T: Decoder]: Decoder[List[T]] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, List[T]] =
      decodeSeq[T](data)

  given [T: Decoder]: Decoder[Vector[T]] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Vector[T]] =
      decodeSeq[T](data).map(_.toVector)

  given [T: Decoder: ClassTag]: Decoder[Array[T]] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Array[T]] =
      decodeSeq[T](data).map(_.toSeq.toArray)

  given [T](using innerDecoder: Decoder[T]): Decoder[Option[T]] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Option[T]] =
      innerDecoder.decode(data) match
        case Right(v) => Right(Some(v))
        case Left(e)  => Right(None)

  given Decoder[Boolean] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Boolean] =
      if data.isBoolean then Right(data.asBoolean.get)
      else decodeError("Json.JBool", data)

  given Decoder[BigInt] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, BigInt] =
      data.asNumber.flatMap(_.toBigInt) match {
        case Some(n) => Right(n)
        case None    => decodeError("JsonNumber.JBigInt", data)
      }

  given Decoder[BigDecimal] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, BigDecimal] =
      data.asNumber.flatMap(_.toBigDecimal) match {
        case Some(n) => Right(n)
        case None    => decodeError("JsonNumber.BigDecimal", data)
      }

  given Decoder[Float] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Float] =
      data.asNumber.map(_.toFloat) match {
        case Some(n) => Right(n)
        case None    => decodeError("JsonNumber.float", data)
      }

  given Decoder[Double] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Double] =
      data.asNumber.map(_.toDouble) match {
        case Some(n) => Right(n)
        case None    => decodeError("JsonNumber.double", data)
      }

  given Decoder[Int] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Int] =
      data.asNumber.flatMap(_.toInt) match {
        case Some(n) => Right(n)
        case None    => decodeError("JsonNumber.int", data)
      }

  given Decoder[Long] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, Long] =
      data.asNumber.flatMap(_.toLong) match {
        case Some(n) => Right(n)
        case None    => decodeError("JsonNumber.long", data)
      }

  given Decoder[String] with
    def decode(
        data: Json,
        
    ): Either[DecodeException, String] =
      data.asString match {
        case Some(n) => Right(n)
        case None    => decodeError("JsonNumber.string", data)
      }

  given Decoder[LocalDateTime] with {
    def decode(
        data: Json,
        
    ): Either[DecodeException, LocalDateTime] = {
      val dt = data.asString.map(str => {
        LocalDateTime.parse(str, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
      })
      dt match {
        case Some(n) => Right(n)
        case None    => decodeError("datetime ISO_LOCAL_DATE_TIME format", data)
      }
    }
  }
  given Decoder[ZonedDateTime] with {
    def decode(
        data: Json,
        
    ): Either[DecodeException, ZonedDateTime] = {
      val dt = data.asString.map(str => {
        ZonedDateTime.parse(str, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      })
      dt match {
        case Some(n) => Right(n)
        case None => decodeError("datetime ISO_OFFSET_DATE_TIME format", data)
      }
    }
  }
  given Decoder[Throwable] with {
    def decode(
        data: Json,
        
    ): Either[DecodeException, Throwable] = {
      data.asString.map(Throwable(_)) match {
        case Some(o) => Right(o)
        case None    => decodeError("Throwable repr must be string, got:", data)
      }
    }
  }
  given [T](using td: Decoder[T]): Decoder[Try[T]] with {
    def decode(
        data: Json,
        
    ): Either[DecodeException, Try[T]] = {
      data.asObject.map(o => {
        if (o.contains("success")) {
          td.decode(o("success").get).map(Try(_))
        } else if (o.contains("failure")) {
          val result = for {
            j <- o("failure")
            s <- j.asString
            result = Throwable(s)
          } yield result
          result match
            case None => decodeError("try.failure must be string , got ", data)
            case Some(value) => Right(Failure(value))
        } else {
          decodeError("Throwable repr must be string, got:", data)
        }
      }) match
        case None        => decodeError("try decode error, got:", data)
        case Some(value) => value
    }
  }

class DecodeException(val message: String) extends Exception(message)
