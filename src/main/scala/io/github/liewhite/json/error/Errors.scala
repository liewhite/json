package io.github.liewhite.json.error

enum JsonErrorType{
  case EncodeError
  case DecodeError
}

case class JsonError(val eType: JsonErrorType, val msg: String) extends Exception(eType.toString + ":" + msg)