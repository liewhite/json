package io.github.liewhite.json

import org.junit.*
import io.github.liewhite.json.codec.*
import io.github.liewhite.json.annotations.*
import io.github.liewhite.json.JsonBehavior.*

import io.circe.Json

case class P1(@Rename("A") a: Int)

class TestPlugins{
  @Test
  def rename = {
    val data = P1(123)
    val j = data.encode
    val s = j.noSpaces
    assert(s == """{"A":123}""")
    val recover = parseToJson(s).toOption.get.decode[P1].toOption.get
    assert(recover == data)
  }
}
