package api

// Based on https://blog.daniel-beskin.com/2025-04-14-whiteboxish-named-tuples
trait BuilderGeneratorSimplest[T](using tc: BuilderTypeClass[T]) extends Selectable {
  import BuilderGeneratorSimplest.*

  import scala.NamedTuple.Split

  type Fields = BuilderFor[Tuple.Head[Split[Tup[T], 1]], Tuple.Last[Split[Tup[T], 1]], T]
  inline def selectDynamic(name: String): Any =
    tc.builder
}

import scala.NamedTuple.Map

import hearth.*
import hearth.fp.syntax.*
import scala.quoted.*

object BuilderGeneratorSimplest {
  import NamedTuple.*
  type Tup[T] = NamedTuple.From[T]

  type BuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case EmptyTuple =>
      NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => T]]
      case h *: t =>
      NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] =>
        BuilderFor[Tuple.Head[Split[R, 1]], Tuple.Last[Split[R, 1]], T]]]

    }
  
  inline given build[T]: BuilderTypeClass[T] = 
    ${ buildImpl[T] }
  
  def buildImpl[T: Type](using Quotes): Expr[BuilderTypeClass[T]] = {
    import quotes.reflect.*
    
    val tpe = TypeRepr.of[T]
    val companion = tpe.typeSymbol.companionModule
    
    val applyMethod = companion.declaredMethods
      .find(_.name == "apply")
      .getOrElse(report.errorAndAbort(s"No apply method found for ${tpe.show}"))
    
    val companionRef = Ref(companion)
    val applySelect = companionRef.select(applyMethod)
    val methodType = applySelect.tpe.widen
    
    // Eta-expand the method
    def etaExpand(prefix: Term, tpe: TypeRepr): Term = {
      tpe match {
        case MethodType(paramNames, paramTypes, returnType) =>
          Lambda(
            owner = Symbol.spliceOwner,
            tpe = MethodType(paramNames)(_ => paramTypes, _ => returnType),
            rhsFn = (sym, args) => {
              val termArgs = args.map(_.asInstanceOf[Term])
              val applied = prefix.appliedToArgs(termArgs)
              
              returnType match {
                case mt: MethodType => etaExpand(applied, mt)
                case _ => applied
              }
            }
          )
          
        case PolyType(paramNames, paramTypes, returnType) =>
          etaExpand(prefix, returnType)
          
        case _ => prefix
      }
    }
    
    val etaExpanded = etaExpand(applySelect, methodType)
    
    // Count the number of parameters
    def countParams(tpe: TypeRepr): Int = tpe match {
      case MethodType(params, _, _) => params.length
      case _ => 0
    }
    
    val paramCount = countParams(methodType)
    
    // Generate the transformation at compile time
    if (paramCount == 0) {
      report.errorAndAbort(s"No parameters found in apply method for ${tpe.show}")
    } else if (paramCount == 1) {
      // Special case for single parameter - no transformation needed
      '{ BuilderTypeClass[T](${ etaExpanded.asExprOf[Any] }.asInstanceOf[Function1[?, ?]]) }
    } else {
      // Generate the nested Tuple1 transformation
      val curriedExpr = etaExpanded.asExprOf[Any]
      
      '{ 
        BuilderTypeClass[T](
          transformCurried($curriedExpr, ${ Expr(paramCount) }).asInstanceOf[Function1[?, ?]]
        )
      }
    }
  }
  
  // Runtime helper to transform curried functions into nested Tuple1 structures
  private def transformCurried(f: Any, arity: Int): Any = {
    if (arity == 1) {
      f
    } else {
      // Apply curried and then wrap in nested Tuple1s
      val curried = f match {
        case f2: Function2[?, ?, ?] => f2.curried
        case f3: Function3[?, ?, ?, ?] => f3.curried
        case f4: Function4[?, ?, ?, ?, ?] => f4.curried
        case f5: Function5[?, ?, ?, ?, ?, ?] => f5.curried
        case f6: Function6[?, ?, ?, ?, ?, ?, ?] => f6.curried
        case f7: Function7[?, ?, ?, ?, ?, ?, ?, ?] => f7.curried
        case f8: Function8[?, ?, ?, ?, ?, ?, ?, ?, ?] => f8.curried
        case f9: Function9[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => f9.curried
        case _ => throw new MatchError(s"Unsupported function arity: ${f.getClass}")
      }
      
      // Wrap in nested Tuple1s
      wrapInTuple1s(curried.asInstanceOf[Function1[?, ?]], arity - 1)
    }
  }
  
  // Recursively wrap function in Tuple1s
  private def wrapInTuple1s(f: Function1[?, ?], depth: Int): Function1[?, ?] = {
    if (depth == 1) {
      f.andThen(result => Tuple1(result))
    } else {
      f.andThen { next =>
        Tuple1(wrapInTuple1s(next.asInstanceOf[Function1[?, ?]], depth - 1))
      }
    }
  } 
}
