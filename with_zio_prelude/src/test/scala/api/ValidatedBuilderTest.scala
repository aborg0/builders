package api

import models.{JavaOptionalDummy, MapContainer, NamedInner, NestedOuter, OptionalInputDummy, PathAwareDummy, PlainPathDummy, SeqContainer, SetContainer, SetPlainContainer, Simple, SimpleValidated, TrailingOptionalDummy}
import utest.*
import playground.Opaque
import zio.prelude.ZValidation

import scala.reflect.Selectable.reflectiveSelectable
import java.time.LocalDate
import api.ValidatedBuilderGenerator.ValidatedBuilder

object ValidatedBuilderTest extends TestSuite {

  val tests = Tests {
    test("TripleVal with manual implementation") {
      import playground.{TripleVal, TripleValO}
      
      // Test successful validation
      val result1 = TripleValO().i(3).op("Op").v("Op").s(2)
      assert(result1.isSuccess)
      
      // Test failed validation - invalid op
      val result2 = TripleValO().i(3).op("NotOp").v("Op").s(2)
      assert(result2.isFailure)
      
      // Test failed validation - invalid sequence number
      val result3 = TripleValO().i(3).op("Op").v("Op").s(-1)
      assert(result3.isFailure)
    }
    
    test("Simple case class with no validation") {
      val result: ZValidation[Nothing, String, Simple] =
        ValidatedBuilderGenerator.builder[Simple].i(2).s("@@").d(LocalDate.of(2026, 1, 24))
      assert(result.isSuccess)
    }

    test("SimpleValidated builder exposes named fields") {
      val builder = SimpleValidated.validator
      val afterI  = builder.i(42)
      val result: ZValidation[Nothing, String, SimpleValidated] = afterI.op("Op")

      assert(result.isSuccess)
      val sv1 = result.toEither.toOption.get
      assert(sv1.i == 42)
    }
    
    test("SimpleValidated with validation") {
      val validator = SimpleValidated.validator
      val result1 = validator.i(42).op("Op")
      assert(result1.isSuccess)
      val sv1 = result1.toEither.toOption.get
      assert(sv1.i == 42)

      val result2 = validator.i(99).op("NotOp")
      assert(result2.isFailure)
    }

    test("SimpleValidated rejects empty op string") {
      val validator = SimpleValidated.validator
      val result = validator.i(10).op("")
      assert(result.isFailure)
    }

    test("SimpleValidated preserves opaque value semantics") {
      val validator = SimpleValidated.validator
      val result = validator.i(1).op("Op")
      assert(result.isSuccess)

      val sv = result.toEither.toOption.get
      val direct = Opaque.Op("Op")
      assert(direct.isRight)
      val directOp = direct.toOption.get

      assert(sv.op == directOp)
    }

    test("Simple builder compiles to expected curried shape") {
      val d = LocalDate.of(2026, 1, 24)
      val result: ZValidation[Nothing, String, Simple] =
        ValidatedBuilderGenerator.builder[Simple].i(0).s("x").d(d)
      assert(result.isSuccess)
      val s = result.toEither.toOption.get
      assert(s.i == 0)
      assert(s.s == "x")
      assert(s.d == d)
    }

    test("builderTyped provides typed staged entrypoint") {
      val result: ZValidation[Nothing, String, Simple] =
        ValidatedBuilderGenerator.builderTyped[Simple].i(5).s("typed").d(LocalDate.of(2026, 1, 24))

      assert(result.isSuccess)
      val built = result.toEither.toOption.get
      assert(built.i == 5)
      assert(built.s == "typed")
      assert(built.d == LocalDate.of(2026, 1, 24))
    }

    test("builderNoAllowTyped preserves smart-constructor validation") {
      val ok = ValidatedBuilderGenerator.builderNoAllowTyped[SimpleValidated].i(7).op("Op")
      assert(ok.isSuccess)

      val bad = ValidatedBuilderGenerator.builderNoAllowTyped[SimpleValidated].i(7).op("NotOp")
      assert(bad.isFailure)
    }

    test("SimpleValidated builder returns ZValidation with String error type") {
      val validator = SimpleValidated.validator
      val result: ZValidation[Nothing, String, SimpleValidated] = validator.i(123).op("NotOp")

      assert(result.isFailure)

      val errors = result.toEither.left.toOption.get
      assert(errors.nonEmpty)
    }

    test("SimpleValidated builder is referentially transparent (no hidden state)") {
      val validator = SimpleValidated.validator
      val res1 = validator.i(1).op("Op")
      val res2 = validator.i(1).op("Op")

      // Same inputs -> both success, and results equal
      assert(res1.isSuccess)
      assert(res2.isSuccess)
      assert(res1.toEither == res2.toEither)
    }

    test("Path-aware builder keeps backward compatibility in default mode") {
      val result: ZValidation[Nothing, String, PathAwareDummy] =
        PathAwareDummy.defaultValidator.name("inner").right(43)

      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.nonEmpty)
      assert(errors.head == "43 is not 42.")
    }

    test("Path-aware builder returns configured path segments") {
      val result: ZValidation[Nothing, ValidationPathError[String], PathAwareDummy] =
        PathAwareDummy.validator.name("inner").right(43)

      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.nonEmpty)
      val first = errors.head
      assert(first.path == Seq(
        ValidationPathPart.Custom("custom prefix"),
        ValidationPathPart.Field("right")
      ))
      assert(first.error == "43 is not 42.")
    }

    test("Path-aware builder prepends outer path to nested builder failures") {
      val result: ZValidation[Nothing, ValidationPathError[String], NestedOuter] =
        NestedOuter.validator.name("outer").child(43).right(42)

      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.nonEmpty)
      val first = errors.head
      assert(first.path == Seq(
        ValidationPathPart.Custom("outer prefix"),
        ValidationPathPart.Field("child"),
        ValidationPathPart.Custom("inner prefix"),
        ValidationPathPart.Field("value")
      ))
      assert(first.error == "43 is not 42.")
    }

    test("Path-aware builder preserves distinct paths for multiple failures") {
      val result: ZValidation[Nothing, ValidationPathError[String], NestedOuter] =
        NestedOuter.validator.name("outer").child(41).right(43)

      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.size == 2)
      assert(errors.contains(
        ValidationPathError(
          Seq(
            ValidationPathPart.Custom("outer prefix"),
            ValidationPathPart.Field("child"),
            ValidationPathPart.Custom("inner prefix"),
            ValidationPathPart.Field("value")
          ),
          "41 is not 42."
        )
      ))
      assert(errors.contains(
        ValidationPathError(
          Seq(
            ValidationPathPart.Custom("outer prefix"),
            ValidationPathPart.Field("right")
          ),
          "43 is not 42."
        )
      ))
    }

    test("Path config is a no-op for all-plain case classes") {
      val result: ZValidation[Nothing, Nothing, PlainPathDummy] =
        PlainPathDummy.validator.name("plain").count(2)

      assert(result.isSuccess)
      val built = result.toEither.toOption.get
      assert(built == PlainPathDummy("plain", 2))
    }

    test("Path-aware Seq field uses index and placeholder Named for failed element") {
      val one = NamedInner.make("alpha", 41)
      val two = NamedInner.make("beta", 42)

      val result: ZValidation[Nothing, ValidationPathError[String], SeqContainer] =
        SeqContainer.validator.name("outer").items(Seq(one, two))

      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.size == 1)
      val first = errors.head
      assert(first.path == Seq(
        ValidationPathPart.Field("items"),
        ValidationPathPart.Index(ValidationPathIndex.wrap(0)),
        ValidationPathPart.Named(None),
        ValidationPathPart.Custom("inner prefix"),
        ValidationPathPart.Field("value")
      ))
      assert(first.error == "41 is not 42.")
    }

    test("Path-aware Seq field preserves distinct indexes for multiple failures") {
      val one = NamedInner.make("alpha", 41)
      val two = NamedInner.make("beta", 43)

      val result: ZValidation[Nothing, ValidationPathError[String], SeqContainer] =
        SeqContainer.validator.name("outer").items(Seq(one, two))

      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.size == 2)
      assert(errors.contains(
        ValidationPathError(
          Seq(
            ValidationPathPart.Field("items"),
            ValidationPathPart.Index(ValidationPathIndex.wrap(0)),
            ValidationPathPart.Named(None),
            ValidationPathPart.Custom("inner prefix"),
            ValidationPathPart.Field("value")
          ),
          "41 is not 42."
        )
      ))
      assert(errors.contains(
        ValidationPathError(
          Seq(
            ValidationPathPart.Field("items"),
            ValidationPathPart.Index(ValidationPathIndex.wrap(1)),
            ValidationPathPart.Named(None),
            ValidationPathPart.Custom("inner prefix"),
            ValidationPathPart.Field("value")
          ),
          "43 is not 42."
        )
      ))
    }

    test("Path-aware Map field appends key Named segment") {
      val result: ZValidation[Nothing, ValidationPathError[String], MapContainer] =
        MapContainer.validator.name("outer").parts(
          Map("left" -> NamedInner.make("alpha", 41))
        )

      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.size == 1)
      val first = errors.head
      assert(first.path == Seq(
        ValidationPathPart.Field("parts"),
        ValidationPathPart.Index(ValidationPathIndex.wrap(0)),
        ValidationPathPart.Named(None),
        ValidationPathPart.Named(Some("left")),
        ValidationPathPart.Custom("inner prefix"),
        ValidationPathPart.Field("value")
      ))
      assert(first.error == "41 is not 42.")
    }

    test("Set field with plain values succeeds without runtime casts") {
      val result: ZValidation[Nothing, Nothing, SetPlainContainer] =
        SetPlainContainer.validator.name("outer").items(Set(1, 2, 3))

      assert(result.isSuccess)
      val built = result.toEither.toOption.get
      assert(built.items == Set(1, 2, 3))
    }

    test("Path-aware Set field accepts pre-validated inputs and accumulates failures") {
      val result: ZValidation[Nothing, ValidationPathError[String], SetContainer] =
        SetContainer.validator.name("outer").items(
          Set(NamedInner.make("alpha", 41))
        )

      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.size == 1)
      val first = errors.head
      assert(first.path == Seq(
        ValidationPathPart.Field("items"),
        ValidationPathPart.Index(ValidationPathIndex.wrap(0)),
        ValidationPathPart.Named(None),
        ValidationPathPart.Custom("inner prefix"),
        ValidationPathPart.Field("value")
      ))
      assert(first.error == "41 is not 42.")
    }

    test("builder accepts raw and None for Option fields") {
      val rawValue = OptionalInputDummy.validator.name("a").maybe(3)
      assert(rawValue.isSuccess)
      assert(rawValue.toEither.toOption.get == OptionalInputDummy("a", Some(3)))

      val noneValue = OptionalInputDummy.validator.name("a").maybe(None)
      assert(noneValue.isSuccess)
      assert(noneValue.toEither.toOption.get == OptionalInputDummy("a", None))
    }

    test("builder supports trailing completion for nullable and Option fields") {
      val result = TrailingOptionalDummy.validator.name("ok").i(2).date(null).result(None)
      assert(result.isSuccess)
      val built = result.toEither.toOption.get
      assert(built.name == "ok")
      assert(built.i == 2)
      assert(built.date == null)
      assert(built.result == None)
    }

    test("builder supports raw values for java Optional family") {
      val result = JavaOptionalDummy.validator
        .name("x")
        .maybe("v")
        .maybeInt(1)
        .maybeLong(2L)
        .maybeDouble(3.0)

      assert(result.isSuccess)
      val built = result.toEither.toOption.get
      assert(built.maybe.isPresent)
      assert(built.maybe.get == "v")
      assert(built.maybeInt.isPresent)
      assert(built.maybeInt.getAsInt == 1)
      assert(built.maybeLong.isPresent)
      assert(built.maybeLong.getAsLong == 2L)
      assert(built.maybeDouble.isPresent)
      assert(built.maybeDouble.getAsDouble == 3.0)
    }

    test("builder completion fills java Optional empties") {
      val result = JavaOptionalDummy.validator
        .name("x")
        .maybe("v")
        .maybeInt(java.util.OptionalInt.empty())
        .maybeLong(java.util.OptionalLong.empty())
        .maybeDouble(java.util.OptionalDouble.empty())
      assert(result.isSuccess)
      val built = result.toEither.toOption.get
      assert(built.maybe.isPresent)
      assert(built.maybe.get == "v")
      assert(!built.maybeInt.isPresent)
      assert(!built.maybeLong.isPresent)
      assert(!built.maybeDouble.isPresent)
    }
  }
}
