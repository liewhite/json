package io.github.liewhite.json

import io.circe.Json
import io.circe.parser.parse
import io.circe.ParsingFailure

def parseString(s:String): Either[ParsingFailure, Json] = {
  parse(s)
}