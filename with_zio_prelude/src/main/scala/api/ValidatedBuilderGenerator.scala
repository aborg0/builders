package api

import api.ValidatorInfo.ValidationKind.{FromEither, FromValidation}
import scala.quoted.*
import scala.NamedTuple.AnyNamedTuple
import zio.prelude.ZValidation

// ─── Top-level: outside the object so LambdaLift can always reference them ────

/** Selectable wrapper exposing builder fields by name at a given remaining level. */
class ValidatedBuilderSelectable[T, E, R <: AnyNamedTuple](
  private[api] val underlying: Any
) extends Selectable {
  type Fields = ValidatedBuilderGenerator.BuilderFields[R, T, E]
  def selectDynamic(name: String): Any = underlying.asInstanceOf[Tuple1[Any]]._1
}

/**
 * Runtime builder chain.  Holds:
 *  - `validators`: one `Any => ZValidation[Nothing,Any,Any]` per field
 *  - `combine`:    `Array[Any] => ZValidation[Nothing,Any,T]`
 *  - `collected`:  parameters supplied so far (one per level consumed)
 *
 * Each level is wrapped in a `Tuple1(fn)` where `fn: Any => <next level or ZValidation>`.
 * Intermediate levels produce a new `ValidatedBuilderSelectable` wrapping the next `Tuple1`.
 * The last level applies all validators and calls `combine`.
 */
class ValidatedBuilderChain[T](
  private val validators: Array[Any => Any],  // Any => ZValidation[Nothing, Any, Any]
  private val combine:    Array[Any] => Any,  // Array[ZValidation] => ZValidation[Nothing, Any, T]
  private val n:          Int,
  private val collected:  Array[Any]          // ZValidation values collected so far
) {
  /**
   * Returns the Tuple1(fn) for level `idx`.
   * `fn` takes the raw field value, validates it, and either:
   *  - wraps the next level in a `ValidatedBuilderSelectable` (intermediate), or
   *  - calls `combine` (last level).
   */
  def tuple1AtLevel[R <: AnyNamedTuple](idx: Int): Tuple1[Any => Any] =
    Tuple1 { (rawParam: Any) =>
      val zv = validators(idx)(rawParam)                      // ZValidation[Nothing, Any, Any]
      val newCollected = collected :+ zv
      if (idx == n - 1) {
        combine(newCollected)                                  // ZValidation[Nothing, Any, T]
      } else {
        val nextChain = new ValidatedBuilderChain[T](validators, combine, n, newCollected)
        // The R type param is erased at runtime; the compile-time Fields type is set by
        // the macro via a Typed ascription on the ValidatedBuilderSelectable constructor call.
        new ValidatedBuilderSelectable[T, Any, R](nextChain.tuple1AtLevel[R](idx + 1))
      }
    }
}

// ─── Main generator ───────────────────────────────────────────────────────────

trait ValidatedBuilderGenerator[T] {
  type Builder
  def apply(): Builder
}

object ValidatedBuilderGenerator {
  import scala.NamedTuple
  import scala.NamedTuple.*
  import scala.NamedTuple.Split
  import zio.prelude.ZValidation
  import api.ValidatorInfo.ValidationKind

  type Tup[T] = NamedTuple.From[T]
  import scala.quoted.*

  // ─── Match types ──────────────────────────────────────────────────────────────

  type BuilderFields[R <: AnyNamedTuple, T, E] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case Tuple1[h] =>
        NamedTuple[NamedTuple.Names[R], Tuple1[h => ZValidation[Nothing, E, T]]]
      case h *: t =>
        NamedTuple[
          Tuple1[Tuple.Head[NamedTuple.Names[R]]],
          Tuple1[h => ValidatedBuilderSelectable[T, E,
            NamedTuple[Tuple.Tail[NamedTuple.Names[R]], t]]]
        ]
    }

  // Legacy alias
  type ValidatedBuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T, E] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case EmptyTuple =>
        NamedTuple[NamedTuple.Names[H],
          Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => ZValidation[Nothing, E, T]]]
      case h *: t =>
        NamedTuple[NamedTuple.Names[H],
          Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => ValidatedBuilderFor[
            Tuple.Head[Split[R,1]], Tuple.Last[Split[R,1]], T, E]]]
    }

  type ValidatedBuilder[T, E] = ValidatedBuilderFor[
    Tuple.Head[Split[Tup[T], 1]], Tuple.Last[Split[Tup[T], 1]], T, E]

  // ─── Public entry points ──────────────────────────────────────────────────────

  inline def derived[T]: ValidatedBuilderGenerator[T]        = ${ derivedImplNoAllow[T] }
  inline def derivedNoAllow[T]: ValidatedBuilderGenerator[T] = ${ derivedImplNoAllow[T] }
  inline def derivedAllow[T]: ValidatedBuilderGenerator[T]   = ${ derivedImplAllow[T] }

  /** Returns `ValidatedBuilderSelectable[T, EU, R]` — a concrete class, so the transparent
   *  inline exposes it without any recursive match-type expansion at the call site. */
  transparent inline def builder[T]        = ${ builderImplNoAllow[T] }
  transparent inline def builderNoAllow[T] = ${ builderImplNoAllow[T] }
  transparent inline def builderAllow[T]   = ${ builderImplAllow[T] }

  // ─── Macro implementations ────────────────────────────────────────────────────

  def derivedImplAllow[T: Type](using Quotes): Expr[ValidatedBuilderGenerator[T]]   = derivedImpl[T](true)
  def derivedImplNoAllow[T: Type](using Quotes): Expr[ValidatedBuilderGenerator[T]] = derivedImpl[T](false)

  def derivedImpl[T: Type](allowUnion: Boolean)(using Quotes): Expr[ValidatedBuilderGenerator[T]] = {
    import quotes.reflect.*
    val (tpe, sym, infos, euRepr) = analyse[T](allowUnion)
    val sel = buildSel(euRepr, tpe, sym, infos, allowUnion)
    tpe.asType match {
      case '[t] =>
        '{ new ValidatedBuilderGenerator[t] {
             type Builder = AnyNamedTuple
             def apply(): Builder = $sel.asInstanceOf[Builder]
           }
        }.asExprOf[ValidatedBuilderGenerator[T]]
      case _ => report.errorAndAbort("Cannot match T")
    }
  }

  def builderImplAllow[T: Type](using Quotes): Expr[Any]   = builderImpl[T](true)
  def builderImplNoAllow[T: Type](using Quotes): Expr[Any] = builderImpl[T](false)

  def builderImpl[T: Type](allowUnion: Boolean)(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val (tpe, sym, infos, euRepr) = analyse[T](allowUnion)
    buildSel(euRepr, tpe, sym, infos, allowUnion)
  }

  // ─── Analysis ─────────────────────────────────────────────────────────────────

  private def analyse[T: Type](allowUnion: Boolean)(using Quotes)
      : (quotes.reflect.TypeRepr, quotes.reflect.Symbol, List[ValidatorInfo], quotes.reflect.TypeRepr) = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol
    if (!sym.flags.is(Flags.Case)) report.errorAndAbort(s"${tpe.show} must be a case class")
    val fields = sym.primaryConstructor.paramSymss.flatten.map(p => (p.name, tpe.memberType(p)))
    if (fields.isEmpty) report.errorAndAbort(s"${tpe.show} must have at least one field")
    val infos  = fields.map { case (n, t) => SmartConstructorDiscovery.discoverValidator(n, t) }
    // Debug: log discovered validator infos
    try {
      infos.zip(fields).foreach { case (info, (n, t)) =>
        info match {
          case nv: ValidatorInfo.NeedsValidation =>
            try {
              MacroDebugger.log(s"Analyse: field=$n NeedsValidation prim=${nv.primitiveType.asInstanceOf[quotes.reflect.TypeRepr].show} wrapped=${nv.wrappedType.asInstanceOf[quotes.reflect.TypeRepr].show} companion=${nv.companionSymbol.asInstanceOf[quotes.reflect.Symbol].fullName} method=${nv.methodName}")
            } catch { case _: Throwable => MacroDebugger.log(s"Analyse: field=$n NeedsValidation <unprintable>") }
          case nv: ValidatorInfo.NoValidation =>
            try { MacroDebugger.log(s"Analyse: field=$n NoValidation plain=${nv.plainType.asInstanceOf[quotes.reflect.TypeRepr].show}") } catch { case _: Throwable => MacroDebugger.log(s"Analyse: field=$n NoValidation <unprintable>") }
        }
      }
    } catch { case _: Throwable => MacroDebugger.log("Analyse: failed to log infos") }
    (tpe, sym, infos, computeUnifiedErrorType(infos))
  }

  // ─── Core: build the outermost ValidatedBuilderSelectable ────────────────────

  /**
   * Generates:
   *   new ValidatedBuilderSelectable[T, EU, R](
   *     new ValidatedBuilderChain[T](validators, combine, n, emptyArray).tuple1AtLevel(0)
   *   )
   *
   * `validators` and `combine` are quoted expressions computed at macro time.
   * `ValidatedBuilderChain` is a top-level class, so LambdaLift can always reference it.
   * No Term-level Lambda or TypeRepr references appear inside generated lambdas.
   */
  private def buildSel(using Quotes)(
    euRepr:     quotes.reflect.TypeRepr,
    targetTpe:  quotes.reflect.TypeRepr,
    targetSym:  quotes.reflect.Symbol,
    infos:      List[ValidatorInfo],
    allowUnion: Boolean
  ): Expr[Any] = {
    import quotes.reflect.*

    val n = infos.length
    if (n > 22) report.errorAndAbort(s"Arity $n > 22")

    val names      = infos.map(_.fieldName)
    val primTypes  = infos.map(computePrimType(_, allowUnion))
    val fieldTypes = infos.map(fieldTypeOf)
    val rType      = ntRepr(names, primTypes)
    val selTC      = TypeRepr.of[ValidatedBuilderSelectable[Any, Any, AnyNamedTuple]].typeSymbol.typeRef
    val selType    = selTC.appliedTo(List(targetTpe, euRepr, rType))
    val selCtor    = TypeRepr.of[ValidatedBuilderSelectable[Any, Any, AnyNamedTuple]]
      .typeSymbol.primaryConstructor

    // Build per-field validator: Any => ZValidation[Nothing, Any, Any]
    val validatorExprs: List[Expr[Any => Any]] = infos.map { info =>
      val primRepr    = computePrimType(info, allowUnion)
      val wrappedRepr = fieldTypeOf(info)
      primRepr.asType match {
        case '[p] => wrappedRepr.asType match {
          case '[w] => euRepr.asType match {
            case '[eu] =>
              val fv: Expr[p => ZValidation[Nothing, eu, w]] = fieldValidator[p, eu, w](info, allowUnion)
              '{ (x: Any) => ($fv)(x.asInstanceOf[p]) }
            case _ => report.errorAndAbort("eu")
          }
          case _ => report.errorAndAbort("w")
        }
        case _ => report.errorAndAbort("p")
      }
    }

    // Build the combine function: Array[ZValidation[Nothing,Any,Any]] => ZValidation[Nothing,Any,T]
    val combineExpr: Expr[Array[Any] => Any] = buildCombine(euRepr, targetTpe, targetSym, fieldTypes)

    // Build the chain expression — purely quoted, no Term-level Lambda
    val nExpr = Expr(n)
    val validatorsExpr: Expr[Array[Any => Any]] = '{ ${ Expr.ofList(validatorExprs) }.toArray }

    euRepr.asType match {
      case '[eu] => targetTpe.asType match {
        case '[t] =>
          val chainExpr: Expr[Tuple1[Any => Any]] = '{
            new ValidatedBuilderChain[t]($validatorsExpr, $combineExpr, $nExpr, new Array[Any](0))
              .tuple1AtLevel[AnyNamedTuple](0)
              .asInstanceOf[Tuple1[Any => Any]]
          }
          // Wrap in ValidatedBuilderSelectable with precise compile-time type
          Typed(
            New(Inferred(selType)).select(selCtor)
              .appliedToTypes(List(targetTpe, euRepr, rType))
              .appliedTo(chainExpr.asTerm),
            Inferred(selType)
          ).asExpr
        case _ => report.errorAndAbort("t")
      }
      case _ => report.errorAndAbort("eu")
    }
  }

  // ─── Per-field validator ─────────────────────────────────────────────────────

  private def fieldValidator[P: Type, EU: Type, W: Type](using Quotes)(
    info:       ValidatorInfo,
    allowUnion: Boolean
  ): Expr[P => ZValidation[Nothing, EU, W]] = {
    import quotes.reflect.*

    def isCheckable(tp: TypeRepr): Boolean = {
      val s = tp.widen.dealias.typeSymbol
      if (s == Symbol.noSymbol) return false
      if (s.flags.is(Flags.Opaque)) return false
      // Newtype/Subtype: the inner `Type` alias is erased to the underlying type at runtime,
      // so a `case x: W` test would match the primitive too — always run validation instead.
      val dealiased = tp.widen.dealias
      if (dealiased.typeSymbol.name == "Type") return false
      // Also check if W's dealiased show ends in ".Type" (covers SequenceNumber.Type etc.)
      try {
        val show = dealiased.show
        if (show.endsWith(".Type")) return false
      } catch { case _: Throwable => () }
      true
    }

    info match {
      case nv: ValidatorInfo.NeedsValidation =>
        val rawPrim = nv.primitiveType.asInstanceOf[TypeRepr]
        val wrapped = TypeRepr.of[W]
        if (allowUnion && !(rawPrim =:= wrapped) && isCheckable(wrapped)) {
          rawPrim.asType match {
            case '[rawP] =>
              '{ (p: P) =>
                (p: Any) match {
                  case x: W => ZValidation.succeed(x.asInstanceOf[W])
                  case _    => ${ smartCall[EU, W](nv, '{ p.asInstanceOf[rawP] }) }
                }
              }
            case _ => report.errorAndAbort("Cannot match rawPrim for union")
          }
        } else {
          // Delegate to smartCall which builds the companion invocation term safely
          rawPrim.asType match {
            case '[rawP] =>
              '{ (p: P) => ${ smartCall[EU, W](nv, '{ p.asInstanceOf[rawP] }) } }
            case _ => report.errorAndAbort(s"Cannot match rawPrim ${rawPrim.show}")
          }
        }
      case _: ValidatorInfo.NoValidation =>
        '{ (p: P) => ZValidation.succeed(p.asInstanceOf[W]) }
    }
  }

  // ─── Combine validators ───────────────────────────────────────────────────────

  /**
   * Builds `Array[ZValidation[Nothing,Any,Any]] => ZValidation[Nothing,eu,T]`.
   * Uses `map` (n=1), `zipWithPar` (n=2), or `foldLeft`+`map` (n≥3, error-accumulating).
   */
  private def buildCombine(using Quotes)(
    euRepr:    quotes.reflect.TypeRepr,
    targetTpe: quotes.reflect.TypeRepr,
    targetSym: quotes.reflect.Symbol,
    fieldTypes: List[quotes.reflect.TypeRepr]
  ): Expr[Array[Any] => Any] = {
    import quotes.reflect.*
    val n      = fieldTypes.length
    val companionModuleSym = targetSym.companionModule
    try MacroDebugger.log(s"buildCombine: targetSym=${targetSym.fullName} companionModule=${companionModuleSym.fullName}") catch { case _: Throwable => () }
    // local search for an accessible 'apply' declared on the companion or related modules
    val applyM_opt_local: Option[(quotes.reflect.Symbol, quotes.reflect.Symbol)] = try {
      import quotes.reflect.*
      def methodsOf(sym: Symbol): List[Symbol] = try sym.declaredMethods.toList catch { case _: Throwable => List.empty }
      val candidates: List[Symbol] = {
        val base = companionModuleSym
        val modClassCompanion = try if (companionModuleSym.moduleClass.exists) companionModuleSym.moduleClass.companionModule else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
        val req1 = try Symbol.requiredModule(companionModuleSym.fullName) catch { case _: Throwable => Symbol.noSymbol }
        val req2 = try Symbol.requiredModule(companionModuleSym.fullName + "$") catch { case _: Throwable => Symbol.noSymbol }
        val req3 = try Symbol.requiredModule(companionModuleSym.fullName.stripSuffix("$")) catch { case _: Throwable => Symbol.noSymbol }
        val ownerBased = try Symbol.requiredModule(companionModuleSym.owner.fullName + "." + companionModuleSym.name) catch { case _: Throwable => Symbol.noSymbol }
        List(base, modClassCompanion, req1, req2, req3, ownerBased).filter(s => s != Symbol.noSymbol).distinct
      }
      // For each candidate, collect its methods and pair method -> owner
      val methodOwnerPairs: List[(Symbol, Symbol)] = candidates.flatMap { cand =>
        methodsOf(cand).map(m => (m, cand)) ++
        (try if (cand.moduleClass.exists) methodsOf(cand.moduleClass).map(m => (m, cand)) else List.empty catch { case _: Throwable => List.empty })
      }
      val accessible = methodOwnerPairs.filter { case (m, owner) => m.name == "apply" && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) }
      accessible.headOption
    } catch { case _: Throwable => None }
    val (applyM, applyOwner) = applyM_opt_local.getOrElse({
      try {
        val objNames = try companionModuleSym.declaredMethods.map(_.name).mkString(",") catch { case _: Throwable => "<error>" }
        val classNames = try if (companionModuleSym.moduleClass.exists) companionModuleSym.moduleClass.declaredMethods.map(_.name).mkString(",") else "<no moduleClass>" catch { case _: Throwable => "<error>" }
        MacroDebugger.log(s"apply lookup failed: companion=${companionModuleSym.fullName} objMethods=$objNames classMethods=$classNames")
      } catch { case _: Throwable => () }
      report.errorAndAbort(s"Could not find accessible 'apply' on companion ${companionModuleSym.fullName}")
    })
    try MacroDebugger.log(s"buildCombine: selected apply owner=${applyOwner.fullName} applySym=${applyM.name}") catch { case _: Throwable => () }
    val compRef = Ref(applyOwner)
    // Find the actual method symbol on the chosen owner to avoid mismatched owner/method symbols
    val applyMethodOnOwner: quotes.reflect.Symbol = try {
      val own = applyOwner
      val methodsObj = try own.declaredMethods.toList catch { case _: Throwable => List.empty }
      val classMethods = try if (own.moduleClass.exists) own.moduleClass.declaredMethods.toList else List.empty catch { case _: Throwable => List.empty }
      (methodsObj ++ classMethods).find(_.name == applyM.name).getOrElse(applyM)
    } catch { case _: Throwable => applyM }
    euRepr.asType match {
      case '[eu] => targetTpe.asType match {
        case '[t] =>
          n match {
            case 1 =>
              fieldTypes(0).asType match {
                case '[f0] =>
                  '{ (arr: Array[Any]) =>
                    arr(0).asInstanceOf[ZValidation[Nothing, eu, f0]].map(a0 =>
                      ${ compRef.select(applyMethodOnOwner).appliedTo('a0.asTerm).asExprOf[t] }
                    )
                  }
                case _ => report.errorAndAbort("f0")
              }
            case 2 =>
              (fieldTypes(0).asType, fieldTypes(1).asType) match {
                case ('[f0], '[f1]) =>
                  '{ (arr: Array[Any]) =>
                    arr(0).asInstanceOf[ZValidation[Nothing, eu, f0]]
                      .zipWithPar(arr(1).asInstanceOf[ZValidation[Nothing, eu, f1]])((a0, a1) =>
                        ${ compRef.select(applyMethodOnOwner).appliedToArgs(List('a0.asTerm, 'a1.asTerm)).asExprOf[t] }
                      )
                  }
                case _ => report.errorAndAbort("f0/f1")
              }
            case _ =>
              // ≥3 fields: accumulate with foldLeft+zipWithPar then map
              val applyFn: Expr[Array[Any] => t] = buildApply[t](targetSym, n, fieldTypes)
              '{ (arr: Array[Any]) =>
                val zvList = arr.toList.map(_.asInstanceOf[ZValidation[Nothing, eu, Any]])
                val combined = zvList.tail.foldLeft(
                  zvList.head.map(List(_))
                )((acc, zv) => acc.zipWithPar(zv)((list, v) => list :+ v))
                combined.map(args => $applyFn(args.toArray))
              }
          }
        case _ => report.errorAndAbort("t")
      }
      case _ => report.errorAndAbort("eu")
    }
  }

  // ─── Apply function for combining results ─────────────────────────────────────

  private def buildApply[T: Type](using Quotes)(
    targetSym:  quotes.reflect.Symbol,
    n:          Int,
    fieldTypes: List[quotes.reflect.TypeRepr]
  ): Expr[Array[Any] => T] = {
    import quotes.reflect.*
    val companionModuleSym2 = targetSym.companionModule
    val applyPairOpt2: Option[(quotes.reflect.Symbol, quotes.reflect.Symbol)] = try {
      import quotes.reflect.*
      def methodsOf(sym: Symbol): List[Symbol] = try sym.declaredMethods.toList catch { case _: Throwable => List.empty }
      val candidates: List[Symbol] = {
        val base = companionModuleSym2
        val modClassCompanion = try if (companionModuleSym2.moduleClass.exists) companionModuleSym2.moduleClass.companionModule else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
        val req1 = try Symbol.requiredModule(companionModuleSym2.fullName) catch { case _: Throwable => Symbol.noSymbol }
        val req2 = try Symbol.requiredModule(companionModuleSym2.fullName + "$") catch { case _: Throwable => Symbol.noSymbol }
        val req3 = try Symbol.requiredModule(companionModuleSym2.fullName.stripSuffix("$")) catch { case _: Throwable => Symbol.noSymbol }
        val ownerBased = try Symbol.requiredModule(companionModuleSym2.owner.fullName + "." + companionModuleSym2.name) catch { case _: Throwable => Symbol.noSymbol }
        List(base, modClassCompanion, req1, req2, req3, ownerBased).filter(s => s != Symbol.noSymbol).distinct
      }
      val methodOwnerPairs: List[(Symbol, Symbol)] = candidates.flatMap { cand =>
        methodsOf(cand).map(m => (m, cand)) ++
        (try if (cand.moduleClass.exists) methodsOf(cand.moduleClass).map(m => (m, cand)) else List.empty catch { case _: Throwable => List.empty })
      }
      val accessible = methodOwnerPairs.filter { case (m, owner) => m.name == "apply" && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) }
      accessible.headOption
    } catch { case _: Throwable => None }
    val (applyM, applyOwner) = applyPairOpt2.getOrElse({
      try {
        val objNames = try companionModuleSym2.declaredMethods.map(_.name).mkString(",") catch { case _: Throwable => "<error>" }
        val classNames = try if (companionModuleSym2.moduleClass.exists) companionModuleSym2.moduleClass.declaredMethods.map(_.name).mkString(",") else "<no moduleClass>" catch { case _: Throwable => "<error>" }
        MacroDebugger.log(s"apply lookup failed (buildApply): companion=${companionModuleSym2.fullName} objMethods=$objNames classMethods=$classNames")
      } catch { case _: Throwable => () }
      report.errorAndAbort(s"Could not find accessible 'apply' on companion ${companionModuleSym2.fullName}")
    })
    try MacroDebugger.log(s"buildApply: selected apply owner=${applyOwner.fullName} applySym=${applyM.name}") catch { case _: Throwable => () }
    val compRef = Ref(applyOwner)
    // Find the actual method symbol on the chosen owner to avoid mismatched owner/method symbols
    val applyMethodOnOwner: quotes.reflect.Symbol = try {
      val own = applyOwner
      val methodsObj = try own.declaredMethods.toList catch { case _: Throwable => List.empty }
      val classMethods = try if (own.moduleClass.exists) own.moduleClass.declaredMethods.toList else List.empty catch { case _: Throwable => List.empty }
      (methodsObj ++ classMethods).find(_.name == applyM.name).getOrElse(applyM)
    } catch { case _: Throwable => applyM }
    val lam = Lambda(
      Symbol.spliceOwner,
      MethodType(List("args"))(_ => List(TypeRepr.of[Array[Any]]), _ => TypeRepr.of[T]),
      (_, ps) => {
        val argsRef = ps.head.asInstanceOf[Term]
        val argTerms = fieldTypes.zipWithIndex.map { case (ft, i) =>
          val elem = Apply(Select(argsRef, TypeRepr.of[Array[Any]].typeSymbol.declaredMethods
            .find(_.name == "apply").get), List(Literal(IntConstant(i))))
          Typed(elem, Inferred(ft))
        }
        compRef.select(applyMethodOnOwner).appliedToArgs(argTerms)
      }
    )
    lam.asExprOf[Array[Any] => T]
  }

  // ─── Helpers ─────────────────────────────────────────────────────────────────

  private def smartCall[EU: Type, W: Type](using Quotes)(
    nv: ValidatorInfo.NeedsValidation, primExpr: Expr[Any]
  ): Expr[ZValidation[Nothing, EU, W]] = {
    import quotes.reflect.*
    val csRaw = nv.companionSymbol.asInstanceOf[quotes.reflect.Symbol]
    try MacroDebugger.log(s"smartCall: companion=${csRaw.fullName} method=${nv.methodName} prim=${nv.primitiveType.asInstanceOf[quotes.reflect.TypeRepr].show} wrapped=${nv.wrappedType.asInstanceOf[quotes.reflect.TypeRepr].show}") catch { case _: Throwable => MacroDebugger.log(s"smartCall: <unprintable companion> method=${nv.methodName}") }

    def methodExistsOn(sym: Symbol, name: String): Boolean = {
      try {
        val methodsObj = sym.declaredMethods.map(_.name)
        val classMethods = try if (sym.moduleClass.exists) sym.moduleClass.declaredMethods.map(_.name) else List.empty catch { case _: Throwable => List.empty }
        (methodsObj ++ classMethods).contains(name)
      } catch { case _: Throwable => false }
    }

    def resolveTerm(sym: Symbol): Symbol = {
      try MacroDebugger.log(s"resolveTerm: trying for symbol=${sym.fullName} flags=${sym.flags}") catch { case _: Throwable => () }
      try if (sym.flags.is(Flags.Module)) return sym catch { case _: Throwable => () }
      try {
        val c = sym.companionModule
        if (c != Symbol.noSymbol && c.flags.is(Flags.Module)) return c
      } catch { case _: Throwable => () }
      try {
        val mc = if (sym.moduleClass.exists) sym.moduleClass else Symbol.noSymbol
        if (mc != Symbol.noSymbol) {
          val mcComp = try mc.companionModule catch { case _: Throwable => Symbol.noSymbol }
          if (mcComp != Symbol.noSymbol && mcComp.flags.is(Flags.Module)) return mcComp
        }
      } catch { case _: Throwable => () }
      try {
        val fn = sym.fullName
        val trials = List(fn, fn + "$", fn.stripSuffix("$"))
        trials.view.flatMap { name =>
          try {
            val m = Symbol.requiredModule(name)
            if (m != Symbol.noSymbol && m.flags.is(Flags.Module)) Some(m) else None
          } catch { case _: Throwable => None }
        }.headOption.getOrElse(sym)
      } catch { case _: Throwable => sym }
    }

    /** Build a Term for `moduleTerm.methodName(arg)`, looking up the method
     *  via the module's type (so inherited methods like `make` on Newtype subobjects
     *  are found correctly and the generated Select has the right owner). */
    def applyMethodOnModule(moduleTerm: Term, methodName: String, arg: Term): Term = {
      // Look up the method on the static type of the module term (includes inherited methods)
      val modTpe = moduleTerm.tpe
      val methodSym = modTpe.typeSymbol.methodMember(methodName).headOption
        .orElse(modTpe.typeSymbol.memberMethod(methodName).headOption)
        .getOrElse {
          // fallback: Select.unique (may still work for some cases)
          return Apply(Select.unique(moduleTerm, methodName), List(arg))
        }
      Apply(Select(moduleTerm, methodSym), List(arg))
    }

    val callTerm: quotes.reflect.Term = try {
      val originalSym = csRaw
      val arg = Typed(primExpr.asTerm, Inferred(nv.primitiveType.asInstanceOf[quotes.reflect.TypeRepr]))
      val resolved = resolveTerm(originalSym)
      try MacroDebugger.log(s"smartCall: resolved term symbol=${resolved.fullName} flags=${resolved.flags}") catch { case _: Throwable => () }

      if (resolved != Symbol.noSymbol && resolved.flags.is(Flags.Module) && !resolved.name.endsWith("$")) {
        // Use the recorded method name; fall back to alternatives if not directly declared
        val chosen =
          if (methodExistsOn(resolved, nv.methodName)) nv.methodName
          else List("make", "apply").find(nm => methodExistsOn(resolved, nm)).getOrElse(nv.methodName)
        applyMethodOnModule(Ref(resolved), chosen, arg)
      } else {
        def buildTermFromFullName(full: String): Option[quotes.reflect.Term] = {
          try {
            val parts = full.split('.').toList
            if (parts.isEmpty) return None
            def clean(s: String): String = s.stripPrefix("_$").stripSuffix("$")
            (parts.length - 1 to 1 by -1).view.flatMap { i =>
              val ownerName = parts.take(i).mkString(".")
              val nested = parts.drop(i).map(clean)
              try {
                val ownerModule = Symbol.requiredModule(ownerName)
                if (ownerModule == Symbol.noSymbol) None
                else {
                  val term0: Term = Ref(ownerModule)
                  val finalTerm = nested.foldLeft[Term](term0) { (acc, seg) => Select.unique(acc, seg) }
                  Some(finalTerm)
                }
              } catch { case _: Throwable => None }
            }.headOption
          } catch { case _: Throwable => None }
        }

        buildTermFromFullName(originalSym.fullName) match {
          case Some(objTerm) =>
            val termSym = objTerm.symbol
            val chosenName =
              if (methodExistsOn(termSym.asInstanceOf[Symbol], nv.methodName)) nv.methodName
              else List("make", "apply").find(nm => methodExistsOn(termSym.asInstanceOf[Symbol], nm)).getOrElse(nv.methodName)
            try applyMethodOnModule(objTerm, chosenName, arg)
            catch { case _: Throwable => Apply(Select.unique(Ref(originalSym), nv.methodName), List(arg)) }
          case None => applyMethodOnModule(Ref(originalSym), nv.methodName, arg)
        }
      }
    } catch {
      case ex: Throwable =>
        import quotes.reflect.*
        MacroDebugger.log(s"smartCall: final fallback due to ${ex.getMessage}")
        Apply(Select.unique(Ref(csRaw), nv.methodName), List(Typed(primExpr.asTerm, Inferred(nv.primitiveType.asInstanceOf[quotes.reflect.TypeRepr]))))
    }

    nv.validationKind match {
      case FromEither =>
        val eitherExpr = callTerm.asExprOf[Either[EU, W]]
        '{ zio.prelude.ZValidation.fromEither($eitherExpr).asInstanceOf[ZValidation[Nothing, EU, W]] }
      case FromValidation =>
        callTerm.asExprOf[ZValidation[Nothing, EU, W]]
    }
  }

  private def computeUnifiedErrorType(using Quotes)(infos: List[ValidatorInfo]): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    infos.collect { case nv: ValidatorInfo.NeedsValidation => nv.errorType.asInstanceOf[TypeRepr] }
      .distinct match {
      case Nil      => TypeRepr.of[Nothing]
      case h :: Nil => h
      case hs       => hs.reduce(OrType(_, _))
    }
  }

  private def computePrimType(using Quotes)(info: ValidatorInfo, allowUnion: Boolean): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    info match {
      case nv: ValidatorInfo.NeedsValidation =>
        val p = nv.primitiveType.asInstanceOf[TypeRepr]
        val w = nv.wrappedType.asInstanceOf[TypeRepr]
        if (allowUnion && !(p.widen.dealias =:= w.widen.dealias)) OrType(p, w) else p
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }
  }

  private def fieldTypeOf(using Quotes)(info: ValidatorInfo): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    info match {
      case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation    => nv.plainType.asInstanceOf[TypeRepr]
    }
  }

  private def ntRepr(using Quotes)(names: List[String], types: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    type SampleNT = scala.NamedTuple.NamedTuple[Tuple1["x"], Tuple1[Int]]
    val ntTC: TypeRepr = TypeRepr.of[SampleNT] match {
      case AppliedType(tc, _) => tc
      case o => report.errorAndAbort(s"Cannot extract NamedTuple TC from ${o.show}")
    }
    val consTC: TypeRepr = TypeRepr.of[Int *: EmptyTuple] match {
      case AppliedType(tc, _) => tc
      case o => report.errorAndAbort(s"Cannot extract *: TC from ${o.show}")
    }
    val ns = names.foldRight[TypeRepr](TypeRepr.of[EmptyTuple])((n, acc) =>
      AppliedType(consTC, List(ConstantType(StringConstant(n)), acc)))
    val vs = types.foldRight[TypeRepr](TypeRepr.of[EmptyTuple])((t, acc) =>
      AppliedType(consTC, List(t, acc)))
    AppliedType(ntTC, List(ns, vs))
  }

  @deprecated("Use macro-generated builder", "now")
  def caseclass1[T, T0, E](f: T0 => ZValidation[Nothing, E, T]): Tuple1[T0 => ZValidation[Nothing, E, T]] = Tuple1(f)
  @deprecated("Use macro-generated builder", "now")
  def caseclass2[T, T0, T1, E](f: (T0, T1) => ZValidation[Nothing, E, T]) = Tuple1(f.curried.andThen(Tuple1(_)))
  @deprecated("Use macro-generated builder", "now")
  def caseclass3[T, T0, T1, T2, E](f: (T0, T1, T2) => ZValidation[Nothing, E, T]) = Tuple1(f.curried.andThen(t => Tuple1(t.andThen(Tuple1(_)))))
  @deprecated("Use macro-generated builder", "now")
  def caseclass4[T, T0, T1, T2, T3, E](f: (T0, T1, T2, T3) => ZValidation[Nothing, E, T]) = Tuple1(f.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1(_)))))))
  @deprecated("Use macro-generated builder", "now")
  def caseclass5[T, T0, T1, T2, T3, T4, E](f: (T0, T1, T2, T3, T4) => ZValidation[Nothing, E, T]) = Tuple1(f.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1(_)))))))))
}




