package io.github.liewhite.json.annotations
import scala.quoted.*
import io.circe.Json
import io.circe.JsonObject

trait ObjEncodeAnnotation{
  def afterEncode(data: Json): Json
}

trait ObjDecodeAnnotation{
  def beforeDecode(data: Json): Json
}

// 传入一切可能需要的参数， 比如外层数据， 
class SnakeCase extends scala.annotation.StaticAnnotation with ObjEncodeAnnotation with ObjDecodeAnnotation {
  def afterEncode(data: Json): Json = {
    val snake = (for {
      obj <- data.asObject
    } yield obj).map(item => {
      item.toMap.map{
        case (k,v) => (toSnake(k),v)
      }
    }).map(item => {
      Json.fromJsonObject(JsonObject.fromMap(item))
    })
    snake match {
      case Some(v) => v
      case None => throw Exception("to snake case error:" + data.noSpaces)
    }
  }

  def beforeDecode(data: Json): Json = {
    val caml = (for {
      obj <- data.asObject
    } yield obj).map(item => {
      item.toMap.map{
        case (k,v) => (toCamel(k),v)
      }
    }).map(item => {
      Json.fromJsonObject(JsonObject.fromMap(item))
    })
    caml match {
      case Some(v) => v
      case None => throw Exception("to caml case error:" + data.noSpaces)
    }
  }

  def toSnake(s: String): String = {
    val len = s.length
    s.zipWithIndex.foldLeft("")((acc, item) => {
      if(item._1.isUpper) {
        if(item._2 == 0 || item._2 == len - 1) {
          acc + item._1.toLower.toString
        }else{
          acc + "_" + item._1.toLower.toString
        }
      }else{
        acc + item._1.toString
      }
    })
  }
  def toCamel(s: String): String = {
    val split = s.split("_")
    val tail = split.tail.map { x => s"${x.head.toUpper}${x.tail}" }
    split.head + tail.mkString
  }
}

