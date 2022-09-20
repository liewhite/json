package io.github.liewhite.json.annotations
import scala.quoted.*
import io.circe.Json
import io.github.liewhite.json.codec.DecodeException

trait FieldEncodeAnnotation{
  def afterEncode(key: String, data: Json): Json
}

trait FieldDecodeAnnotation{
  def beforeDecode(key: String, data: Json): Json
}

class Flatten extends scala.annotation.StaticAnnotation with FieldEncodeAnnotation with FieldDecodeAnnotation {
  def afterEncode(key: String, data: Json): Json = {
    (for {
      obj <- data.asObject
      item <- obj(key)
      toFlatten <- item.asObject
    } yield obj.remove(key).deepMerge(toFlatten)) match {
      case Some(v) => Json.fromJsonObject(v)
      case None => throw Exception(s"flatten json error: can't merge $key 's value to ${data.noSpaces}")
    }
  }

  def beforeDecode(key: String, data: Json): Json = {
    (for {
      obj <- data.asObject
    } yield obj.add(key, data)) match {
      case Some(v) => Json.fromJsonObject(v)
      case None => throw DecodeException(s"expand json error: can't add $key  to ${data.noSpaces}")
    }
  }
}



