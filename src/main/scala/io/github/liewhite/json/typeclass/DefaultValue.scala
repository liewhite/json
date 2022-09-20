package io.github.liewhite.json.typeclass

import scala.compiletime.*
import scala.quoted.*
import io.circe.Json
import io.circe.syntax.*
import io.github.liewhite.json.codec.Encoder
import io.github.liewhite.json.JsonBehavior.*

/**
 * 
 * 默认值只影响 decode 和 json schema
 **/
trait DefaultValue[T]:
  def defaults: Map[String,Json]

object DefaultValue{
  inline given [T]: DefaultValue[T] = mkDefaultValue[T]

  inline def mkDefaultValue[T]: DefaultValue[T] =
    ${mkDefaultValueMacro[T]}

  def mkDefaultValueMacro[T: Type](using Quotes): Expr[DefaultValue[T]] = {
    import quotes.reflect.*

    def toJson(expr: (Expr[Any], TypeRepr)):Expr[Json] = {
      expr._2.asType match {
        case '[t] => '{
          summonInline[Encoder[t]].encode(${expr._1.asInstanceOf})
        } 
      }
    }

    try {
      val sym = TypeTree.of[T].symbol
      val comp = sym.companionClass
      // val types = sym.caseFields.map(_.tree.asInstanceOf[ValDef].tpt.tpe)
      val types = 
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.tree.asInstanceOf[ValDef].tpt.tpe
      val names = 
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.name

      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Ref] = 
        for case deff @ DefDef(name, _, _, _) <- body
        if name.startsWith("$lessinit$greater$default")
        yield Ref(deff.symbol)

      val namesExpr: Expr[List[String]] =
        Expr.ofList(names.map(Expr(_)))
      val identsExpr = idents.map(_.asExpr)
      val values = Expr.ofList(identsExpr.zip(types).map(item => toJson(item)))

      '{ mkGiven($namesExpr, $values) }
    }
    catch {
      case _ =>
        '{mkGiven(List.empty[String],List.empty[Json])}
    }

  }
  def mkGiven[T](names: List[String], values: List[Json]): DefaultValue[T] = {
    new DefaultValue:
      def defaults = {
        names.zip(values).toMap
      }
  }
}