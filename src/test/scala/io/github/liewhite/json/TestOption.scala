package com.liewhite.json

import org.junit.*
import io.github.liewhite.json.JsonBehavior.*
import io.github.liewhite.json.codec.*
import java.time.*


case class OptionalField(a: Option[Int])

class TestOption:
  @Test
  def encode = {
    val o = OptionalField(None)
    assert(o.encode.noSpaces == "{}")
  }
  @Test
  def decode = {
    assert("{}".parseToJson.toOption.get.decode[OptionalField].toOption.get == OptionalField(None))
  }