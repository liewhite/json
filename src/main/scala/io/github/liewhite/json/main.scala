package io.github.liewhite.json

import io.github.liewhite.json.JsonBehavior.*

@main def main = {
    val o: Int | String | Boolean = true
    val s = o.encode
    println(s.noSpaces)

    s.decode[Int|Boolean].foreach(println)

}