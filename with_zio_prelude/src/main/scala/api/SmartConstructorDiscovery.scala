package api

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.time.Instant
import scala.quoted.*
import api.ValidatorInfo.*

object MacroDebugger {
  private val logFilePath = "macro_debug.log"
  // Use append mode
  private val writer = new PrintWriter(new BufferedWriter(new FileWriter(logFilePath, true)))

  def log(msg: String): Unit = synchronized {
    writer.println(s"[${Instant.now}] $msg")
    writer.flush()
  }
}

/**
 * Discovers smart constructors for types at compile time.
 * 
 * Discovery priority:
 * 1. companion.apply returning Validation[_, T]
 * 2. companion.make returning Validation[_, T]
 * 3. companion.apply returning Either[_, T]
 * 4. No validation needed (identity)
 * 
 * Note: These are regular functions used during macro expansion, not inline defs.
 */
object SmartConstructorDiscovery {
  
    /**
   * Discovers how to validate a field type.
   * Returns ValidatorInfo describing the validation strategy.
   */
  def discoverValidator(using Quotes)(
    fieldName: String,
    fieldType: quotes.reflect.TypeRepr
  ): ValidatorInfo = {
    import quotes.reflect.*

    // Optional-like unions and stdlib optionals are plain pass-through fields.
    fieldType.widen.dealias match {
      case OrType(a, b) if a.widen.dealias =:= TypeRepr.of[Null] || b.widen.dealias =:= TypeRepr.of[Null] =>
        MacroDebugger.log(s"Nullable union detected for $fieldName — treating as no-validation")
        return ValidatorInfo.NoValidation(fieldName, fieldType)
      case AppliedType(tc, _) if tc.typeSymbol == TypeRepr.of[Option[Any]].typeSymbol =>
        MacroDebugger.log(s"Option detected for $fieldName — treating as no-validation")
        return ValidatorInfo.NoValidation(fieldName, fieldType)
      case AppliedType(tc, _) if tc.typeSymbol == TypeRepr.of[java.util.Optional[Any]].typeSymbol =>
        MacroDebugger.log(s"java.util.Optional detected for $fieldName — treating as no-validation")
        return ValidatorInfo.NoValidation(fieldName, fieldType)
      case t if t =:= TypeRepr.of[java.util.OptionalInt] || t =:= TypeRepr.of[java.util.OptionalLong] || t =:= TypeRepr.of[java.util.OptionalDouble] =>
        MacroDebugger.log(s"java.util.Optional primitive variant detected for $fieldName — treating as no-validation")
        return ValidatorInfo.NoValidation(fieldName, fieldType)
      case _ => ()
    }

    // ── Collection detection (Seq/List/Set/Vector/Map) ──────────────────────────
    // Must happen BEFORE the general scala.* skip since collection types start with "scala.".
    fieldType.widen.dealias match {
      case AppliedType(tc, List(elemType)) if isSeqLikeTC(tc) =>
        MacroDebugger.log(s"SeqLike detected for $fieldName (${fieldType.show}) — discovering element validator")
        val elemInfo    = discoverValidator(fieldName, elemType)
        val nameField   = findNameFieldInType(elemType)
        return ValidatorInfo.SeqLikeValidation(fieldName, fieldType, elemType, elemInfo, nameField)
      case AppliedType(tc, List(keyType, valType)) if isMapTC(tc) =>
        MacroDebugger.log(s"Map detected for $fieldName (${fieldType.show}) — discovering value validator")
        val valueInfo = discoverValidator(fieldName, valType)
        val nameField = findNameFieldInType(valType)
        return ValidatorInfo.MapValidation(fieldName, fieldType, keyType, valType, valueInfo, nameField)
      case _ => ()
    }

    val typeSymbol = fieldType.typeSymbol
    if (typeSymbol == Symbol.noSymbol) {
      MacroDebugger.log(s"No symbol for field '$fieldName' (${fieldType.show}) — treating as no-validation")
      return ValidatorInfo.NoValidation(fieldName, fieldType)
    }
    // Skip searching companion for plain Scala primitive/standard types — they don't have smart constructors.
    try {
      if (typeSymbol.fullName.startsWith("scala.")) {
        MacroDebugger.log(s"Primitive/standard scala type detected (${typeSymbol.fullName}) — skipping companion lookup for $fieldName")
        return ValidatorInfo.NoValidation(fieldName, fieldType)
      }
      if (typeSymbol.fullName.startsWith("java.")) {
        MacroDebugger.log(s"Java standard type detected (${typeSymbol.fullName}) — treating as no-validation for $fieldName")
        return ValidatorInfo.NoValidation(fieldName, fieldType)
      }
    } catch { case _: Throwable => () }

    // If the dealiased type symbol is an inner `Type` alias (e.g. SequenceNumber.Type),
    // prefer the owner module as the companion (zio.prelude Newtype/Subtype pattern).
    try {
      val dealiased = fieldType.widen.dealias
      val dSym = dealiased.typeSymbol
      if (dSym != Symbol.noSymbol && dSym.name == "Type") {
        // Try to extract the concrete owner module from the dealiased type's show string.
        // e.g. "playground.Opaque.SequenceNumber.Type" -> ownerName = "playground.Opaque.SequenceNumber"
        val ftShow = try fieldType.widen.dealias.show catch { case _: Throwable => "" }
        val ownerCandidateOpt: Option[quotes.reflect.Symbol] = try {
          val idx = ftShow.lastIndexOf(".Type")
          if (idx > 0) {
            val ownerName = ftShow.substring(0, idx)
            // Try requiredModule on a few variants
            val variants = List(ownerName, ownerName + "$", ownerName.replace("$.", "."), ownerName.replace("$.", ".") + "$")
            variants.view.flatMap { name =>
              try {
                val m = quotes.reflect.Symbol.requiredModule(name)
                if (m != quotes.reflect.Symbol.noSymbol) Some(m) else None
              } catch { case _: Throwable => None }
            }.headOption
          } else None
        } catch { case _: Throwable => None }

        val moduleCandidate = ownerCandidateOpt.getOrElse {
          // Fallback to previous behavior: use the owner symbol (may be the abstract Subtype/Newtype class)
          val owner = dSym.owner
          try {
            if (owner.flags.is(Flags.Module)) owner
            else if (owner.companionModule.exists) owner.companionModule
            else owner
          } catch { case _: Throwable => owner }
        }
        MacroDebugger.log(s"Detected wrapped alias Type; using companion candidate ${moduleCandidate.fullName} for field $fieldName (original dealiased show: ${ftShow})")
        try { report.info(s"SmartConstructorDiscovery: wrapper Type detected; companionCandidate=${moduleCandidate.fullName} for field=$fieldName") } catch { case _: Throwable => () }
        val found = findValidationMethod(moduleCandidate, fieldType, fieldName)
        if (found.isDefined) return found.get
        // findValidationMethod failed — this can happen for locally-defined Newtype/Subtype where
        // 'make' is inherited and not discoverable via declaredMethods.
        // Last resort: try to build NeedsValidation by probing Select.unique for 'make' directly.
        val localFallback: Option[ValidatorInfo.NeedsValidation] = try {
          // Resolve the primitive type from the Newtype base: Newtype[A] → A
          val primTpe: TypeRepr = try {
            // Try to extract from moduleClass baseTypes looking for NewtypeCustom[A]
            val mc = try if (moduleCandidate.moduleClass.exists) moduleCandidate.moduleClass else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
            val baseTs = try if (mc != Symbol.noSymbol) mc.typeRef.baseClasses else List.empty catch { case _: Throwable => List.empty }
            baseTs.view.flatMap { bc =>
              try {
                val bcTpe = bc.typeRef
                bcTpe.baseType(Symbol.requiredClass("zio.prelude.NewtypeCustom")) match {
                  case AppliedType(_, List(a)) => Some(a)
                  case _ => None
                }
              } catch { case _: Throwable => None }
            }.headOption.getOrElse(TypeRepr.of[Any])
          } catch { case _: Throwable => TypeRepr.of[Any] }

          val modRef = Ref(moduleCandidate)
          val makeSelect = Select.unique(modRef, "make")
          val makeTpe = makeSelect.tpe.widen.dealias
          makeTpe match {
            case mt: MethodType =>
              val paramTpe = mt.paramTypes.headOption.getOrElse(primTpe)
              analyzeReturnType(mt.resType, fieldType) match {
                case Some((errTpe, vk)) =>
                  Some(ValidatorInfo.NeedsValidation(fieldName, paramTpe, fieldType, errTpe, api.MacroUtils.toTermModule(moduleCandidate), "make", vk))
                case None => None
              }
            case PolyType(_, _, result) =>
              analyzeReturnType(result.dealias, fieldType) match {
                case Some((errTpe, vk)) =>
                  Some(ValidatorInfo.NeedsValidation(fieldName, primTpe, fieldType, errTpe, api.MacroUtils.toTermModule(moduleCandidate), "make", vk))
                case None => None
              }
            case _ => None
          }
        } catch { case ex: Throwable =>
          MacroDebugger.log(s"  Local Newtype fallback also failed for $fieldName: ${ex.getMessage}")
          None
        }
        return localFallback.getOrElse(ValidatorInfo.NoValidation(fieldName, fieldType))
      }
    } catch { case _: Throwable => () }

    MacroDebugger.log(s"Discovering validator for field '$fieldName' of type ${fieldType.show}")
    MacroDebugger.log(s"  Type symbol: ${typeSymbol.fullName}")

    // Find the companion object. This is tricky for opaque types.
    val companionOpt: Option[Symbol] =
      if (typeSymbol.companionModule.exists && typeSymbol.companionModule != Symbol.noSymbol) {
        MacroDebugger.log(s"  Found standard companion: ${typeSymbol.companionModule.fullName}")
        Some(typeSymbol.companionModule)
      } else {
        MacroDebugger.log(s"  Standard companion not found for ${typeSymbol.fullName}. Looking for sibling module with name '${typeSymbol.name}'.")
        val owner = typeSymbol.owner

        // Direct sibling lookup (works for regular classes/objects)
        val directSibling = owner.declarations.find(s => (s.name.init == typeSymbol.name || s.name == typeSymbol.name) && s.flags.is(Flags.Module))
        directSibling match {
          case some @ Some(s) =>
            MacroDebugger.log(s"  Found sibling companion module: ${s.fullName}")
            some
          case None =>
            // Fallbacks to support zio.prelude Subtype/NewType patterns where the exposed field
            // type may be an inner alias `X.Type` or simply `Type` whose companion is the enclosing
            // module object (e.g. SequenceNumber.Type -> SequenceNumber object).

            // Helper: try searching upward from a symbol for a module with the given base name
            // This is tolerant to compiler-generated prefixes (e.g. "_$TestNew") and
            // tries to match by simple name or by fullName suffix.
            def searchUpForModule(sym: Symbol, baseName: String): Option[Symbol] = {
              if (sym == Symbol.noSymbol) None
              else {
                val stripped = baseName.stripPrefix("_$").stripSuffix("$Type").stripSuffix("Type")
                val candidates = sym.declarations.collect {
                  case d if d.flags.is(Flags.Module) => d
                }
                // Try exact matches first
                candidates.find(d => d.name == baseName || d.name == baseName.init) match {
                  case some @ Some(_) => some
                  case None =>
                    // Try stripped/simple matches
                    candidates.find(d => d.name == stripped || d.name == stripped.init) match {
                      case some @ Some(_) => some
                      case None =>
                        // Try matching by fullName suffix (handles nested/local modules)
                        candidates.find(d => d.fullName.endsWith("." + stripped) || d.fullName.endsWith("$" + stripped)) match {
                          case some @ Some(_) => some
                          case None => searchUpForModule(sym.owner, baseName)
                        }
                    }
                }
              }
            }

            // If the type's simple name is `Type` (or ends with `Type`), try to use the owner name
            // as the candidate module name (e.g. `SequenceNumber.Type` -> module `SequenceNumber`).
            val typeBasedCandidate: Option[Symbol] =
              if (typeSymbol.name == "Type" || typeSymbol.name.endsWith("Type")) {
                val base = typeSymbol.owner.name
                MacroDebugger.log(s"  Attempting subtype/newtype resolution using owner name: $base")
                searchUpForModule(owner, base)
              } else None

            typeBasedCandidate match {
              case some @ Some(s) =>
                MacroDebugger.log(s"  Found subtype/newtype companion module: ${s.fullName}")
                some
              case None =>
                // Last-resort global search: strip common suffixes and look upward for a matching module
                val stripped = typeSymbol.name.stripSuffix("$Type").stripSuffix("Type")
                MacroDebugger.log(s"  Attempting global search for module matching stripped name: $stripped")
                searchUpForModule(owner, stripped) match {
                  case some @ Some(s) =>
                    MacroDebugger.log(s"  Found global companion: ${s.fullName}")
                    some
                  case None =>
                    MacroDebugger.log(s"  No companion module found for ${typeSymbol.fullName}; trying requiredModule fallbacks")
                    // Try a few possible full-name candidates for local/test-defined modules
                    val candidates = List(
                      typeSymbol.fullName + "$",
                      typeSymbol.owner.fullName + ".$" + typeSymbol.name,
                      typeSymbol.owner.fullName + ".$" + typeSymbol.name + "$",
                      typeSymbol.owner.fullName + "." + typeSymbol.name,
                      typeSymbol.owner.fullName + "." + typeSymbol.name + "$"
                    ).distinct
                    val found = candidates.view.flatMap { fn =>
                      try {
                        val mod = quotes.reflect.Symbol.requiredModule(fn)
                        MacroDebugger.log(s"  requiredModule resolved: $fn -> ${mod.fullName}")
                        Some(mod)
                      } catch { case _: Throwable =>
                        MacroDebugger.log(s"  requiredModule failed: $fn")
                        None
                      }
                    }.headOption
                    found.orElse {
                      MacroDebugger.log(s"  All requiredModule fallbacks failed for ${typeSymbol.fullName}")
                      None
                    }
                }
            }
        }
      }

    companionOpt match {
      case Some(rawCompanion) =>
        // Normalize to module (object) symbol if possible. Many of our lookups
        // require the module symbol so methods declared on the object (or inherited)
        // are visible.
        val companionModule: Symbol = try {
          if (rawCompanion.flags.is(Flags.Module)) rawCompanion
          else if (rawCompanion.companionModule.exists) rawCompanion.companionModule
          else rawCompanion
        } catch { case _: Throwable => rawCompanion }

        MacroDebugger.log(s"  Using companion module: ${companionModule.fullName}")
        try {
          report.info(s"SmartConstructorDiscovery: using companion module ${companionModule.fullName} for field ${fieldName}")
        } catch { case _: Throwable => () }
        findValidationMethod(companionModule, fieldType, fieldName) match {
          case Some(info) =>
            MacroDebugger.log(s"  Found validation for $fieldName: ${info.methodName}")
            try { report.info(s"SmartConstructorDiscovery: found validation for ${fieldName}; method=${info.methodName}") } catch { case _: Throwable => () }
            info
          case None =>
            MacroDebugger.log(s"  No validation method found in companion '${companionModule.fullName}' for $fieldName")
            ValidatorInfo.NoValidation(fieldName, fieldType)
        }
      case None =>
        MacroDebugger.log(s"  No companion object could be resolved for $fieldName.")
        try { report.info(s"SmartConstructorDiscovery: no companion object for ${fieldName} (${fieldType.show})") } catch { case _: Throwable => () }
        ValidatorInfo.NoValidation(fieldName, fieldType)
    }
  }
  
  /**
   * Tries to find a suitable validation method in the companion object.
   *
   * Strategy:
   *  1. Try `apply` on the DIRECT companion only (not zio.prelude base classes) — handles opaque types.
   *  2. Try `make` on all candidates including zio.prelude bases — handles Newtype/Subtype.
   *  3. Try `apply` on all candidates — broader fallback.
   */
  private def findValidationMethod(using Quotes)(
    companion: quotes.reflect.Symbol,
    targetType: quotes.reflect.TypeRepr,
    fieldName: String
  ): Option[ValidatorInfo.NeedsValidation] = {
    import quotes.reflect.*

    // Step 1: `apply` on the direct companion only (no zio.prelude base injection)
    val directApply = findMethodValidation(companion, "apply", targetType, fieldName, directOnly = true)
    if (directApply.isDefined) return directApply

    // Step 2: `make` on all candidates (including zio.prelude bases) — for Newtype/Subtype
    val makeResult = findMethodValidation(companion, "make", targetType, fieldName, directOnly = false)
    if (makeResult.isDefined) return makeResult

    // Step 3: `apply` on all candidates — broader fallback
    findMethodValidation(companion, "apply", targetType, fieldName, directOnly = false)
  }
  
  /**
   * Looks for a method (apply or make) returning Validation or Either.
   * @param directOnly if true, only look at the companion itself (+ its moduleClass/requiredModule variants)
   *                   but NOT zio.prelude base-class candidates.  This avoids opaque-type `apply`
   *                   accidentally matching an inherited `make` from Newtype/Subtype bases.
   */
  private def findMethodValidation(using Quotes)(
     companion: quotes.reflect.Symbol,
     methodName: String,
     targetType: quotes.reflect.TypeRepr,
     fieldName: String,
     directOnly: Boolean = false
   ): Option[ValidatorInfo.NeedsValidation] = {
     import quotes.reflect.*

      // Build an expanded set of candidate symbols to inspect:
      // - the companion object symbol
      // - its moduleClass
      // - its companionModule (if any)
      // - requiredModule fallbacks for possible naming variants
      // - known zio.prelude base classes and their companion/module variants (only when !directOnly)
      val candSyms = scala.collection.mutable.ListBuffer[Symbol]()
      def addIfValid(s: Symbol): Unit = if (s != null && s != Symbol.noSymbol) candSyms += s

      addIfValid(companion)
      try addIfValid(companion.moduleClass) catch { case _: Throwable => () }
      try addIfValid(companion.companionModule) catch { case _: Throwable => () }

      // requiredModule fallbacks for companion fullName
      try {
        val fn = companion.fullName
        List(fn, fn.stripSuffix("$"), fn + "$", fn.replace("$.", ".")).distinct.foreach { name =>
          try addIfValid(Symbol.requiredModule(name)) catch { case _: Throwable => () }
        }
      } catch { case _: Throwable => () }

      // Known zio.prelude bases — only added when directOnly=false (broader search)
      if (!directOnly) {
        List("zio.prelude.Subtype", "zio.prelude.Newtype", "zio.prelude.SubtypeCustom", "zio.prelude.NewtypeCustom").foreach { bn =>
          try {
            val bs = Symbol.requiredClass(bn)
            addIfValid(bs)
            try addIfValid(bs.companionModule) catch { case _: Throwable => () }
            try addIfValid(bs.moduleClass) catch { case _: Throwable => () }
          } catch { case _: Throwable => () }
        }
      }

      val candidates = candSyms.toList.distinct

      def inspectOn(sym: Symbol, name: String, fallbackPrim: TypeRepr, declaredOnly: Boolean = false): Option[ValidatorInfo.NeedsValidation] = {
        try {
          val methodsObj = try sym.declaredMethods.filter(_.name == name) catch { case _: Throwable => List.empty }
          val methodsClass = try if (sym.moduleClass.exists) sym.moduleClass.declaredMethods.filter(_.name == name) else List.empty catch { case _: Throwable => List.empty }
          val methodsAll = (methodsObj ++ methodsClass).distinct
          val fromDeclared = methodsAll.flatMap { method =>
            try {
              val compRef = Ref(sym)
              val sel = Select.unique(compRef, method.name)
              val mTpe = sel.tpe.widen.dealias
              mTpe match {
                case mt: MethodType =>
                  val paramTpe = mt.paramTypes.headOption.getOrElse(fallbackPrim)
                  val returnTpe = mt.resType
                  analyzeReturnType(returnTpe, targetType) match {
                    case Some((errTpe, vk)) =>
                      // Use the method's owner symbol as the companion symbol, normalized to a term module
                      val ownerSym = try method.owner catch { case _: Throwable => sym }
                      val companionSym = try {
                        if (ownerSym.flags.is(Flags.Module)) ownerSym
                        else if (ownerSym.companionModule.exists) ownerSym.companionModule
                        else sym
                      } catch { case _: Throwable => sym }
                      Some(ValidatorInfo.NeedsValidation(fieldName, paramTpe, targetType, errTpe, api.MacroUtils.toTermModule(companionSym), method.name, vk))
                    case None => None
                  }
                case PolyType(_, _, result) =>
                  analyzeReturnType(result.dealias, targetType) match {
                    case Some((errTpe, vk)) =>
                      val ownerSym = try method.owner catch { case _: Throwable => sym }
                      val companionSym = try {
                        if (ownerSym.flags.is(Flags.Module)) ownerSym
                        else if (ownerSym.companionModule.exists) ownerSym.companionModule
                        else sym
                      } catch { case _: Throwable => sym }
                      Some(ValidatorInfo.NeedsValidation(fieldName, fallbackPrim, targetType, errTpe, api.MacroUtils.toTermModule(companionSym), method.name, vk))
                    case None => None
                  }
                case _ => None
              }
            } catch { case _: Throwable => None }
          }.headOption
          // If nothing found among declared methods, attempt a Select.unique(ref, name)
          // BUT only when declaredOnly=false — when true, we intentionally skip inherited methods
          // (e.g. to avoid picking up inherited `apply` from Newtype/Subtype base classes).
          if (declaredOnly) fromDeclared
          else fromDeclared.orElse {
            try {
              val compRef = Ref(sym)
              val sel = Select.unique(compRef, name)
              val mTpe = sel.tpe.widen.dealias
              mTpe match {
                case mt: MethodType =>
                  val paramTpe = mt.paramTypes.headOption.getOrElse(fallbackPrim)
                  val returnTpe = mt.resType
                  analyzeReturnType(returnTpe, targetType) match {
                    case Some((errTpe, vk)) =>
                      Some(ValidatorInfo.NeedsValidation(fieldName, paramTpe, targetType, errTpe, api.MacroUtils.toTermModule(sym), name, vk))
                    case None => None
                  }
                case PolyType(_, _, result) =>
                  analyzeReturnType(result.dealias, targetType) match {
                    case Some((errTpe, vk)) => Some(ValidatorInfo.NeedsValidation(fieldName, fallbackPrim, targetType, errTpe, api.MacroUtils.toTermModule(sym), name, vk))
                    case None => None
                  }
                case _ => None
              }
            } catch { case _: Throwable => None }
          }
        } catch { case _: Throwable => None }
      }

    // Search all candidates for the requested method name.
    // When directOnly=true we skip inherited (Select.unique) fallback to avoid picking up
    // inherited methods from zio.prelude bases on the module under test.
      val result = candidates.view.flatMap(sym => inspectOn(sym, methodName, TypeRepr.of[Any], declaredOnly = directOnly)).headOption
      result
  }

  // ─── Collection helpers ───────────────────────────────────────────────────────

  private def isSeqLikeTC(using Quotes)(tc: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    val sym = tc.typeSymbol
    sym == TypeRepr.of[Seq[Any]].typeSymbol ||
    sym == TypeRepr.of[List[Any]].typeSymbol ||
    sym == TypeRepr.of[scala.collection.immutable.Set[Any]].typeSymbol ||
    sym == TypeRepr.of[Vector[Any]].typeSymbol ||
    sym == TypeRepr.of[scala.collection.Seq[Any]].typeSymbol
  }

  private def isMapTC(using Quotes)(tc: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tc.typeSymbol == TypeRepr.of[Map[Any, Any]].typeSymbol
  }

  /** Returns the field name annotated with `@api.Name` in the primary constructor of `tpe`,
   *  if exactly one such field exists.  Used at macro time to embed Named path segments
   *  for collection element types that carry identity names. */
  private[api] def findNameFieldInType(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[String] = {
    import quotes.reflect.*
    try {
      val sym = tpe.typeSymbol
      if (!sym.flags.is(Flags.Case)) return None
      val params = sym.primaryConstructor.paramSymss.flatten
      val nameAnnotationFqn = "api.Name"
      val marked = params.filter { p =>
        p.annotations.exists { ann =>
          try ann.tpe.typeSymbol.fullName == nameAnnotationFqn
          catch { case _: Throwable => false }
        }
      }
      if (marked.size == 1) Some(marked.head.name) else None
    } catch { case _: Throwable => None }
  }

  /**
   * Analyzes a return type to see if it's Validation[E, T] or Either[E, T].
   * Returns Some((errorType, validationKind)) if valid, None otherwise.
   * 
   * Note: Validation[E, T] is a type alias for ZValidation[Nothing, E, T]
   */
  private def analyzeReturnType(using Quotes)(
    returnType: quotes.reflect.TypeRepr,
    expectedWrappedType: quotes.reflect.TypeRepr
  ): Option[(quotes.reflect.TypeRepr, ValidationKind)] = {
    import quotes.reflect.*
    
    MacroDebugger.log(s"          Analyzing return type: ${returnType.show}")
    MacroDebugger.log(s"          Expected wrapped type: ${expectedWrappedType.show}")
    
    val rt = returnType.widen.dealias
    MacroDebugger.log(s"          Dealiased return type: ${rt.show}")
    rt match {
      case AppliedType(tycon, args) if args.nonEmpty =>
        val tcName = tycon.typeSymbol.fullName
        val tcSimple = tycon.typeSymbol.name
        MacroDebugger.log(s"            Tycon: $tcName / $tcSimple ; args=${args.map(_.show).mkString(",")}")
        if (tcName.contains("ZValidation") || tcSimple.contains("ZValidation")) {
          // ZValidation[log, err, wrapped]
          args match {
            case List(_, errorType, wrappedType) =>
              val wrappedS = wrappedType.simplified
              val expectedS = expectedWrappedType.simplified
              // ownerMatch: supports patterns like SequenceNumber.Type where the wrapped type is an inner Type alias
              val ownerMatch = wrappedS.typeSymbol.name == "Type" && (
                wrappedS.typeSymbol.owner == expectedS.typeSymbol ||
                wrappedS.typeSymbol.owner == expectedS.dealias.typeSymbol ||
                wrappedS.typeSymbol.owner.name == expectedS.typeSymbol.name ||
                wrappedS.typeSymbol.owner.name == expectedS.dealias.typeSymbol.name
              )
              if (wrappedS =:= expectedS || ownerMatch || wrappedS.widen.dealias =:= expectedS.widen.dealias) {
                MacroDebugger.log(s"            SUCCESS: Found ZValidation with matching wrapped type: ${wrappedS.show}")
                Some((errorType, ValidationKind.FromValidation))
              } else {
                MacroDebugger.log(s"            FAILURE: ZValidation wrapped type ${wrappedType.show} does not match expected ${expectedWrappedType.show}")
                None
              }
            case _ => None
          }
        } else if (tcName.contains("Validation") || tcSimple.contains("Validation")) {
          // Validation[E, T] alias
          args match {
            case List(errorType, wrappedType) =>
              val wrappedS = wrappedType.simplified
              val expectedS = expectedWrappedType.simplified
              val ownerMatch = wrappedS.typeSymbol.name == "Type" && (
                wrappedS.typeSymbol.owner == expectedS.typeSymbol ||
                wrappedS.typeSymbol.owner == expectedS.dealias.typeSymbol ||
                wrappedS.typeSymbol.owner.name == expectedS.typeSymbol.name ||
                wrappedS.typeSymbol.owner.name == expectedS.dealias.typeSymbol.name
              )
              if (wrappedS =:= expectedS || ownerMatch || wrappedS.widen.dealias =:= expectedS.widen.dealias) {
                MacroDebugger.log(s"            SUCCESS: Found Validation alias with matching wrapped type: ${wrappedS.show}")
                Some((errorType, ValidationKind.FromValidation))
              } else {
                MacroDebugger.log(s"            FAILURE: Validation alias wrapped type ${wrappedType.show} does not match expected ${expectedWrappedType.show}")
                None
              }
            case _ => None
          }
        } else if (tcName == "scala.util.Either" || tcSimple == "Either") {
          args match {
            case List(errorType, wrappedType) =>
              val wrappedS = wrappedType.simplified
              val expectedS = expectedWrappedType.simplified
              if (wrappedS =:= expectedS || wrappedS.widen.dealias =:= expectedS.widen.dealias) {
                MacroDebugger.log(s"            SUCCESS: Found Either with matching simplified wrapped type: ${wrappedS.show}")
                Some((errorType, ValidationKind.FromEither))
              } else {
                MacroDebugger.log(s"            FAILURE: Either wrapped type ${wrappedType.show} does not match expected ${expectedWrappedType.show}")
                None
              }
            case _ => None
          }
        } else {
          MacroDebugger.log(s"          Return tycon not recognized as Validation/Either: ${tycon.typeSymbol.fullName}")
          None
        }
      case other =>
        MacroDebugger.log(s"          Return type is not an AppliedType recognized as validation: ${other.show}")
        None
    }
  }
  
  /**
   * Creates a Term that calls the validator on a given argument Term.
   * This generates the actual validation call at macro expansion time.
   */
  def createValidationCall(using Quotes)(
     validatorInfo: ValidatorInfo.NeedsValidation,
     argumentTerm: quotes.reflect.Term
   ): quotes.reflect.Term = {
     import quotes.reflect.*

      val nv = validatorInfo
      val primMethodName = nv.methodName

      def tryMethodOn(sym: Symbol): Option[quotes.reflect.Term] = {
        import quotes.reflect.*
        try {
          val mod = api.MacroUtils.toModule(sym)
          // Use Select.unique on the term module symbol — this will resolve methods defined on the module or inherited from its moduleClass.
          Some(Apply(Select.unique(Ref(mod), primMethodName), List(argumentTerm)))
        } catch { case _: Throwable => None }
      }

      // Build candidate symbols to try (prefer what was discovered, then wrappedType owner, then requiredModule fallbacks)
      val candidates: List[Symbol] = try {
        val discovered = nv.companionSymbol.asInstanceOf[quotes.reflect.Symbol]
        val wrappedOwner = try nv.wrappedType.asInstanceOf[quotes.reflect.TypeRepr].typeSymbol.owner catch { case _: Throwable => Symbol.noSymbol }
        val extra = List(discovered, discovered.moduleClass, wrappedOwner, wrappedOwner.moduleClass).filter(s => s != null && s != Symbol.noSymbol).distinct
        // also attempt requiredModule resolutions for owner.fullName if available
        val reqs = try {
          val wn = wrappedOwner.fullName
          List(
            try Symbol.requiredModule(wn) catch { case _: Throwable => Symbol.noSymbol },
            try Symbol.requiredModule(wn + "$") catch { case _: Throwable => Symbol.noSymbol }
          )
        } catch { case _: Throwable => List.empty }
        (extra ++ reqs).filter(_ != Symbol.noSymbol).distinct
      } catch { case _: Throwable => List.empty }

      val methodCallOpt = candidates.view.flatMap(tryMethodOn).headOption
      val methodCall = methodCallOpt.getOrElse {
        import quotes.reflect.*
        // Normalize the stored companion symbol to a term module symbol if possible
        val compRaw = try nv.companionSymbol.asInstanceOf[quotes.reflect.Symbol] catch { case _: Throwable => Symbol.noSymbol }
        val compModule = if (compRaw != Symbol.noSymbol) api.MacroUtils.toTermModule(compRaw) else Symbol.noSymbol

        // Defensive: only emit a direct method call if the companion/module actually exposes the method
        def methodExistsOn(sym: Symbol, name: String): Boolean = {
          try {
            val methodsObj = sym.declaredMethods.map(_.name)
            val classMethods = try if (sym.moduleClass.exists) sym.moduleClass.declaredMethods.map(_.name) else List.empty catch { case _: Throwable => List.empty }
            (methodsObj ++ classMethods).contains(name)
          } catch { case _: Throwable => false }
        }

        if (compModule != Symbol.noSymbol && methodExistsOn(compModule, primMethodName)) {
          try {
            val compTerm = compModule
            Apply(Select.unique(Ref(compTerm), primMethodName), List(argumentTerm))
          } catch { case _: Throwable =>
            // fallback to select unique on the raw companion if it somehow works
            try Apply(Select.unique(Ref(compRaw), primMethodName), List(argumentTerm)) catch { case _: Throwable => createSucceedCall(argumentTerm) }
          }
        } else {
          // If we can't find a method, don't emit an invalid call; treat as successful pass-through
          MacroDebugger.log(s"createValidationCall: no method '$primMethodName' found on companion for validator; returning succeed for argument.")
          createSucceedCall(argumentTerm)
        }
     }

     validatorInfo.validationKind match {
       case ValidationKind.FromValidation =>
         // Smart constructor already returns Validation[E, A]
         // which is a type alias for ZValidation[Nothing, E, A]. Use as-is.
         methodCall

       case ValidationKind.FromEither =>
         // Smart constructor returns Either[E, A]; convert to Validation[E, A]
         // Validation[E, A] is an alias for ZValidation[Nothing, E, A]
         val validationType   = TypeRepr.of[zio.prelude.Validation.type]
         val validationComp   = validationType.typeSymbol.companionModule
         val validationRef    = Ref(validationComp)
         val fromEitherOpt    = validationComp.declaredMethods.find(_.name == "fromEither")
         val fromEitherMethod = fromEitherOpt.getOrElse {
           report.errorAndAbort("Could not find zio.prelude.Validation.fromEither")
         }

         // Extract E and A from Either[E, A]
         val eitherType = methodCall.tpe.widen.dealias
         val (errorType, resultType) = eitherType match {
           case AppliedType(_, List(e, a)) => (e, a)
           case other =>
             report.errorAndAbort(
               s"Expected Either[E, A] return type for validation method, but found: ${other.show}"
             )
         }

         // Call Validation.fromEither[E, A](eitherValue): Validation[E, A]
         validationRef
           .select(fromEitherMethod)
           .appliedToTypes(List(errorType, resultType))
           .appliedTo(methodCall)
     }
   }

  /**
   * Creates a Term that wraps a plain value in ZValidation.succeed.
   * This produces ZValidation[Nothing, String, T] by widening the error channel
   */
  def createSucceedCall(using Quotes)(
    argumentTerm: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    
    // Access ZValidation companion and call succeed
    val zvalidationModule = TypeRepr.of[zio.prelude.ZValidation.type].typeSymbol.companionModule
    val zvalidationRef = Ref(zvalidationModule)
    
    // Get the succeed method
    val succeedMethods = zvalidationModule.declaredMethods.filter(_.name == "succeed")
    val succeedMethod = succeedMethods.head
    
    // Create ZValidation.succeed[A](value) which gives ZValidation[Nothing, Nothing, A]
    val argType = argumentTerm.tpe.widen
    val succeedCall = zvalidationRef
      .select(succeedMethod)
      .appliedToType(argType)
      .appliedTo(argumentTerm)
    
    // Widen the error type from Nothing to String
    // We can call .mapError on the validation to widen the error channel
    // Or use identity function that widens the type
    // Actually, we need to use mapError with an identity-like function
    // Let's use: validation.asInstanceOf[ZValidation[Nothing, String, A]]
    // But that's not safe. Better: use mapError(_ => throw new Exception())
    // which will never be called but widens the type
    
    // Best approach: just use Typed to ascribe the wider type
    val widenedType = TypeRepr.of[zio.prelude.ZValidation]
      .appliedTo(List(TypeRepr.of[Nothing], TypeRepr.of[String], argType))
    
    Typed(
      succeedCall,
      Inferred(widenedType)
    )
  }
}
