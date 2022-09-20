package io.github.liewhite.json

import org.junit.*
import io.github.liewhite.json.typeclass.RepeatableAnnotation
import io.github.liewhite.json.typeclass.RepeatableAnnotations
import io.github.liewhite.json.codec.*

class Ann(val value:String) extends scala.annotation.Annotation
class AnnExtend(value:String) extends Ann(value)

@Ann("hello")
@AnnExtend("world")
case class Target(a:Int) derives Encoder,Decoder


case class AnnsTarget(

  @Ann("hello")
  @AnnExtend("world a")
  a:Int, 

  @Ann("hello")
  @AnnExtend("world b")
  b:Boolean

) derives Encoder,Decoder

class TestAnnotation:
  @Test
  def annotation = 
    val annotations = RepeatableAnnotation[Ann,Target]
    assert(annotations.apply().map(_.value) == List("hello","world"))
  

  @Test
  def annotations = 
    val annotations = RepeatableAnnotations[Ann,AnnsTarget]
    assert(annotations().flatMap(_.map(_.value)) == List("hello","world a","hello","world b"))

