package io.github.liewhite.json

import io.github.liewhite.json.JsonBehavior.*

case class Ta(a: Option[Int])

@main def main = {
    val t = Ta(None)
    println(t.encode.decode[Ta])
}