package com.liewhite.json

import org.junit.*
import io.github.liewhite.json.JsonBehavior.{encode,decode}
import io.github.liewhite.json.codec.*
import java.time.*


class TestEncode:
  @Test
  def local = {
    val datetime = LocalDateTime.now()
    val j = datetime.encode
    assert(j.decode[LocalDateTime].toOption.get.isEqual(datetime))
  }

  @Test
  def zoned = {
    val datetime = ZonedDateTime.now()
    val j = datetime.encode
    assert(j.decode[ZonedDateTime].toOption.get.isEqual(datetime))
  }