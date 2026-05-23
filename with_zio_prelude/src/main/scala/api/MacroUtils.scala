package api

import scala.quoted.*

object MacroUtils {
  private def normalizeTerm(using Quotes)(s: quotes.reflect.Symbol): quotes.reflect.Symbol = {
    import quotes.reflect.*
    try {
      if (s == Symbol.noSymbol) s
      else if (s.flags.is(Flags.Module) && !s.name.endsWith("$")) s
      else {
        // try companionModule
        val comp = try if (s.companionModule.exists) s.companionModule else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
        if (comp != Symbol.noSymbol && comp.flags.is(Flags.Module) && !comp.name.endsWith("$")) return comp
        // try moduleClass.companionModule
        val mc = try if (s.moduleClass.exists) s.moduleClass else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
        val mcComp = if (mc != Symbol.noSymbol) try mc.companionModule catch { case _: Throwable => Symbol.noSymbol } else Symbol.noSymbol
        if (mcComp != Symbol.noSymbol && mcComp.flags.is(Flags.Module) && !mcComp.name.endsWith("$")) return mcComp
        // try requiredModule on stripped name
        val fn = s.fullName
        try {
          val candidate = Symbol.requiredModule(fn.stripSuffix("$"))
          if (candidate != Symbol.noSymbol && candidate.flags.is(Flags.Module) && !candidate.name.endsWith("$")) return candidate
        } catch { case _: Throwable => () }
        s
      }
    } catch { case _: Throwable => s }
  }

  /**
   * Normalize a symbol to the module (object) symbol if possible.
   * Guarantees (when possible) that the returned symbol has Flags.Module (a term symbol)
   */
  def toModule(using Quotes)(sym: quotes.reflect.Symbol): quotes.reflect.Symbol = {
    import quotes.reflect.*
    try {
      if (sym == Symbol.noSymbol) sym
      else {
        val n = normalizeTerm(sym)
        if (n != Symbol.noSymbol) n
        else {
          // fallback to previous heuristic
          if (sym.flags.is(Flags.Module) && !sym.name.endsWith("$")) sym
          else {
            // Prefer companionModule if it is a module (term)
            val comp = try if (sym.companionModule.exists) sym.companionModule else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
            if (comp != Symbol.noSymbol && comp.flags.is(Flags.Module)) return comp
            // If we have a moduleClass, try its companionModule
            val mc = try if (sym.moduleClass.exists) sym.moduleClass else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
            val mcComp = if (mc != Symbol.noSymbol) try mc.companionModule catch { case _: Throwable => Symbol.noSymbol } else Symbol.noSymbol
            if (mcComp != Symbol.noSymbol && mcComp.flags.is(Flags.Module)) return mcComp
            // Try resolving requiredModule by fullName
            val fn = sym.fullName
            try {
              val m1 = Symbol.requiredModule(fn)
              if (m1.flags.is(Flags.Module)) return m1
              val m1Comp = try if (m1.companionModule.exists) m1.companionModule else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
              if (m1Comp != Symbol.noSymbol && m1Comp.flags.is(Flags.Module)) return m1Comp
            } catch { case _: Throwable => () }
            try {
              val m2 = Symbol.requiredModule(fn + "$")
              if (m2.flags.is(Flags.Module)) return m2
              val m2Comp = try if (m2.companionModule.exists) m2.companionModule else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
              if (m2Comp != Symbol.noSymbol && m2Comp.flags.is(Flags.Module)) return m2Comp
            } catch { case _: Throwable => () }
            // Fall back to companion if exists even if not flagged
            if (comp != Symbol.noSymbol) comp
            else sym
          }
        }
      }
    } catch { case _: Throwable => sym }
  }

  /**
   * Ensure we return a term module/object symbol (Flags.Module) where possible.
   * Tries:
   *  - toModule(sym)
   *  - sym.companionModule
   *  - sym.moduleClass.companionModule
   *  - Symbol.requiredModule variants on fullName with replacements
   */
  def toTermModule(using Quotes)(sym: quotes.reflect.Symbol): quotes.reflect.Symbol = {
    import quotes.reflect.*
    try {
      MacroDebugger.log(s"toTermModule: input symbol=${sym.fullName} flags=${sym.flags}")
      var c = toModule(sym)
      // Normalize c to ensure it's a proper term object
      c = normalizeTerm(c)
      MacroDebugger.log(s"toTermModule: toModule returned ${c.fullName} flags=${if (c != Symbol.noSymbol) c.flags.toString else "<no symbol>"}")
      if (c != Symbol.noSymbol && c.flags.is(Flags.Module) && !c.name.endsWith("$")) return c
      // try companionModule on original
      val comp = try if (sym.companionModule.exists) sym.companionModule else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
      val compNorm = normalizeTerm(comp)
      if (compNorm != Symbol.noSymbol && compNorm.flags.is(Flags.Module) && !compNorm.name.endsWith("$")) return compNorm
      // try moduleClass companion
      val mc = try if (sym.moduleClass.exists) sym.moduleClass else Symbol.noSymbol catch { case _: Throwable => Symbol.noSymbol }
      val mcComp = if (mc != Symbol.noSymbol) try mc.companionModule catch { case _: Throwable => Symbol.noSymbol } else Symbol.noSymbol
      val mcCompNorm = normalizeTerm(mcComp)
      if (mcCompNorm != Symbol.noSymbol && mcCompNorm.flags.is(Flags.Module) && !mcCompNorm.name.endsWith("$")) return mcCompNorm

      val fn = sym.fullName
      // If name looks like a module class (ends with $), try stripping the trailing $ and resolving the term module
      val strippedSuffix = if (fn.endsWith("$")) fn.stripSuffix("$") else fn
      val trials = List(strippedSuffix, strippedSuffix + "$", fn, fn + "$", fn.replace("$.", "."), fn.replace("$.", ".") + "$")
      val res = trials.view.flatMap { name =>
        try {
          val m = Symbol.requiredModule(name)
          val mn = normalizeTerm(m)
          if (mn.flags.is(Flags.Module) && !mn.name.endsWith("$")) Some(mn) else None
        } catch { case _: Throwable => None }
      }.headOption.orElse {
        // As a last attempt, search the owner declarations for a module whose simple name matches
        try {
          val base = sym.name.stripSuffix("$").stripPrefix("_$")
          val owner = sym.owner
          owner.declarations.collectFirst { case d if d.flags.is(Flags.Module) && (d.name == base || d.name.stripSuffix("$") == base) => d }
        } catch { case _: Throwable => None }
      }.getOrElse(c)
      val finalRes = normalizeTerm(res)
      try MacroDebugger.log(s"toTermModule: final resolved symbol=${finalRes.fullName} flags=${if (finalRes != Symbol.noSymbol) finalRes.flags.toString else "<no symbol>"}") catch { case _: Throwable => () }
      finalRes
    } catch { case _: Throwable => sym }
  }
}
