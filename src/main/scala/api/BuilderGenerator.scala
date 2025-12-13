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
    
    // Use Hearth's type utilities
    val tpe = TypeRepr.of[T]
    val companion = tpe.typeSymbol.companionModule
    
    // Find the apply method
    val applyMethod = companion.declaredMethods
      .find(_.name == "apply")
      .getOrElse(report.errorAndAbort(s"No apply method found for ${tpe.show}"))
    
    // Get companion object reference and apply method
    val companionRef = Ref(companion)
    val applySelect = companionRef.select(applyMethod)
    val methodType = applySelect.tpe.widen
    
    // Eta-expand the method using Hearth's tree building utilities
    def etaExpand(prefix: Term, tpe: TypeRepr): Term = {
      tpe match {
        case MethodType(paramNames, paramTypes, returnType) =>
          // Create lambda for this parameter list
          Lambda(
            owner = Symbol.spliceOwner,
            tpe = MethodType(paramNames)(_ => paramTypes, _ => returnType),
            rhsFn = (sym, args) => {
              val termArgs = args.map(_.asInstanceOf[Term])
              val applied = prefix.appliedToArgs(termArgs)
              
              returnType match {
                case mt: MethodType =>
                  // More parameter lists, continue eta-expansion
                  etaExpand(applied, mt)
                case _ =>
                  // No more parameter lists
                  applied
              }
            }
          )
          
        case PolyType(paramNames, paramTypes, returnType) =>
          // Skip type parameters and continue
          etaExpand(prefix, returnType)
          
        case _ =>
          // No parameters left
          prefix
      }
    }
    
    // Perform eta-expansion
    val etaExpanded = etaExpand(applySelect, methodType)
    val curriedExpr = etaExpanded.asExprOf[Any]
    
    // Pattern match on the function arity and create BuilderTypeClass
    methodType match {
      case MethodType(params, _, _) if params.nonEmpty =>
        '{
          BuilderTypeClass[T](
            $curriedExpr match {
              case f: Function1[?, ?] => caseclass1(f)
              case f: Function2[?, ?, ?] => caseclass2(f)
              case f: Function3[?, ?, ?, ?] => caseclass3(f)
              case f: Function4[?, ?, ?, ?, ?] => caseclass4(f)
              case f: Function5[?, ?, ?, ?, ?, ?] => caseclass5(f)
              case f: Function6[?, ?, ?, ?, ?, ?, ?] => caseclass6(f)
              case f: Function7[?, ?, ?, ?, ?, ?, ?, ?] => caseclass7(f)
              case f: Function8[?, ?, ?, ?, ?, ?, ?, ?, ?] => caseclass8(f)
              case f: Function9[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => caseclass9(f)
              case other => 
                throw new MatchError(s"Unsupported function arity: ${other.getClass}")
            }
          )
        }
      case _ =>
        report.errorAndAbort(s"No parameters found in apply method for ${tpe.show}")
    }
  }
 
  def caseclass1[T, T0](transform: T0 => T): T0 => T = transform
//  def caseclass2[T, T0, T1](transform: (T0, T1) => T) = (a: T0) => Tuple1((b: T1) => transform.tupled)
  def caseclass2[T, T0, T1](transform: (T0, T1) => T) = transform.curried.andThen((a) => Tuple1(a))
  def caseclass3[T, T0, T1, T2](transform: (T0, T1, T2) => T) = transform.curried.andThen(t => Tuple1(t.andThen(Tuple1.apply)))
  def caseclass4[T, T0, T1, T2, T3](transform: (T0, T1, T2, T3) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))
  def caseclass5[T, T0, T1, T2, T3, T4](transform: (T0, T1, T2, T3, T4) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))
  def caseclass6[T, T0, T1, T2, T3, T4, T5](transform: (T0, T1, T2, T3, T4, T5) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))))
  def caseclass7[T, T0, T1, T2, T3, T4, T5, T6](transform: (T0, T1, T2, T3, T4, T5, T6) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))))))
  def caseclass8[T, T0, T1, T2, T3, T4, T5, T6, T7](transform: (T0, T1, T2, T3, T4, T5, T6, T7) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))))))))
  def caseclass9[T, T0, T1, T2, T3, T4, T5, T6, T7, T8](transform: (T0, T1, T2, T3, T4, T5, T6, T7, T8) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))))))))))
}
