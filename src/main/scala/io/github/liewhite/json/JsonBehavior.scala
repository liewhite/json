package io.github.liewhite.json

import scala.util.NotGiven
import scala.compiletime.summonInline

import io.github.liewhite.json.codec.Encoder
import io.github.liewhite.json.codec.{DecodeException,Decoder}
import io.circe.Json
import io.circe.parser
import io.circe.ParsingFailure


trait JsonExtensions{

  extension [T](t:T) {
    def encode(using encoder:  Encoder[T]): Json =
      encoder.encode(t)
  }

  extension (t:Json) {
    def decode[T](using decoder:Decoder[T]):Either[DecodeException, T] = decoder.decode(t)
  }

  extension (s: String) {
    def parseToJson: Either[ParsingFailure, Json] = parser.parse(s)
  }

}

object JsonBehavior extends JsonExtensions
  