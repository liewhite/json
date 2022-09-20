# Another Json Lib for scala 3
TL;DR:
* based on circe-core (string <-> Json)
* implement union type auto deriving
* enum singleton case encode to string
* plugins support

## Dependency
```scala
libraryDependencies += "io.github.liewhite" %% "json" % "0.2.0"
```

## examples
### Union type
```scala
import io.github.liewhite.json.JsonBehavior.*

@main def main = {
    val o: Int | String | Boolean = true
    val s = o.encode
    println(s.noSpaces)

    s.decode[Int|Boolean].foreach(println)
}
```