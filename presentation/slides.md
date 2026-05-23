---
theme: seriph
title: Expressive Models, Safe Construction
info: |
  Scala ADTs to readable, validated construction in real systems.
class: text-left
highlighter: shiki
lineNumbers: false
drawings:
  enabled: false
transition: slide-left
mdc: true
css: ./styles/index.css
---

# Expressive Models, Safe Construction

<div class="kicker">Model -> Transform -> Construct -> Validate</div>

A Scala story with builders, validation, and readability in MCP-like systems.

- Audience: mixed Scala + platform/API engineers
- Style: conceptual, MCP-oriented examples

---

# Agenda

- ADTs: modeling MCP requests and responses
- Mapping layers: where ducktape/chimney help
- Pattern matching and update ergonomics
- Readable construction with builders
- Validated construction from external input
- Practical adoption checklist

---
layout: section
---

# 1) Why this talk

---

# The Core Thesis

Scala gives us elegant domain modeling.

The hard part starts later:

- constructing values readably
- validating untrusted inputs
- preserving type safety without unreadable glue code

Goal: keep readability and safety together.

---

# Running Example: MCP-ish Tool API

We model one flow:

- external JSON request
- internal typed model
- tool execution result

We will revisit the same flow in every section.

---
layout: section
---

# 2) ADTs are a great start

---

# ADT Modeling in Scala 3

```scala
enum Role {
  case User
  case Assistant
  case Tool
}

enum Content {
  case Text(value: String)
  case Json(value: String)
}

final case class Message(
  role: Role,
  content: Content,
  requestId: String
)

final case class ToolCall(
  name: String,
  argumentsJson: String,
  requestId: String
)
```

Expressive and compact.

---

# MCP Response ADT

```scala
enum ToolResult {
  case Ok(outputJson: String)
  case ValidationFailed(errors: List[String])
  case InternalError(message: String)
}

final case class ToolResponse(
  requestId: String,
  tool: String,
  result: ToolResult
)
```

Enums and case classes describe protocol-level intent very clearly.

---

# Where Friction Appears

Even with nice ADTs, production code still asks:

- how do we map across layers?
- how do we update nested structures?
- how do we construct safely when many fields look similar?
- how do we validate from external sources?

---
layout: section
---

# 3) Transforming between layers

---

# Mapping Pain (Manual)

```scala
final case class ToolCallDto(name: String, argsJson: String, id: String)
final case class ToolCall(name: String, argumentsJson: String, requestId: String)

def toDomain(dto: ToolCallDto): ToolCall = {
  ToolCall(
    name = dto.name,
    argumentsJson = dto.argsJson,
    requestId = dto.id
  )
}
```

Easy at first, repetitive at scale.

---

# Ducktape / Chimney Fit Here

They help with transformation boilerplate:

- DTO -> Domain
- Domain -> API
- renames/defaults/partial transforms

Key positioning for this talk:

They solve mapping.
Builders solve readable construction and validation semantics.

---
layout: section
---

# 4) Matching and updates

---

# Named Pattern Matching Improves Intent

```scala
call match {
  case ToolCall(name = "search", argumentsJson = payload, requestId = reqId) =>
    handleSearch(payload, reqId)
  case ToolCall(name = "fetch", argumentsJson = payload, requestId = reqId) =>
    handleFetch(payload, reqId)
  case ToolCall(name = other, requestId = reqId, argumentsJson = _) =>
    reject(other, reqId)
}
```

Less positional guessing, fewer underscore-heavy patterns.

---

# copy is Excellent, Until Nesting Hurts

```scala
final case class RetryMeta(attempt: Int, lastError: Option[String])
final case class ToolEnvelope(call: ToolCall, meta: RetryMeta)

val updated = envelope.copy(
  meta = envelope.meta.copy(attempt = envelope.meta.attempt + 1)
)
```

Good for shallow updates.
Verbose for deep object graphs.

---

# Optics as a Complement

Use optics when deep updates dominate:

```scala
// Illustrative style with optics-like API
val updated = ToolEnvelopeOptics.meta.attempt.modify(_ + 1)(envelope)
```

Decision rule:

- shallow updates: copy is usually enough
- repetitive deep updates: optics pay off

---
layout: section
---

# 5) Construction readability

---

# Why Constructor Calls Can Be Opaque

```scala
final case class ToolExecutionContext(
  toolName: String,
  requestId: String,
  tenantId: String,
  actorId: String,
  traceId: String
)

val ctx = ToolExecutionContext("search", "req-001", "tenant-a", "assistant", "trace-42")
```

Many fields share the same type.
Readability depends on memorizing field order.

---

# MCP-Oriented Builder Flow

```scala
final case class ToolExecutionContext(
  toolName: String,
  requestId: String,
  tenantId: String,
  actorId: String,
  traceId: String
)

object ToolExecutionContext extends BuilderGeneratorSimplest[ToolExecutionContext]

val ctx = ToolExecutionContext.builder
  .toolName("search")
  .requestId("req-001")
  .tenantId("tenant-a")
  .actorId("assistant")
  .traceId("trace-42")
```

Named fields make call sites readable even without memorizing constructor order.

---

# Staged Construction for Tool Results

```scala
enum ToolStatus {
  case Success
  case Failure
}

final case class ToolResultModel(
  requestId: String,
  tool: String,
  status: ToolStatus,
  payload: String
)

object ToolResultModel extends BuilderGeneratorSimplest[ToolResultModel]

val partial = ToolResultModel.builder.requestId("req-001").tool("search")
val complete = partial.status(ToolStatus.Success).payload("{\"count\":2}")
```

Readable progression without constructor-order coupling.

---

# Private Constructor for Safety

```scala
final case class ApiKey private(value: String)
object ApiKey extends BuilderGeneratorSimplest[ApiKey]

val key = ApiKey.builder.value("mcp-key-prod")
```

When construction is restricted, builder usage becomes the intentional API.


---

# Generic Builder Example for MCP Payload Wrapper

```scala
final case class PayloadBox[A](value: A)
object PayloadBox
object PayloadBoxBuilders extends BuilderGeneratorGeneric1[PayloadBox]

val jsonBox: PayloadBox[String] = PayloadBoxBuilders.builder[String].value("{\"tool\":\"search\"}")
```

Useful when one transport abstraction wraps multiple payload types.

---
layout: section
---

# 6) Validation from external input

---

# External Sources Change the Game

From APIs/queues/files we receive primitives and strings.

Validation code often becomes:

- repetitive
- branch-heavy
- hard to read once nested

We want readable construction with typed validation.

---

# Smart Constructors for MCP Fields

```scala
opaque type ToolName = String
object ToolName {
  def apply(s: String): Either[String, ToolName] =
    s match {
      case "search" | "fetch" | "summarize" => Right(s: ToolName)
      case other => Left(s"Unsupported tool: $other")
    }
}

opaque type RequestId = String
object RequestId {
  def apply(s: String): ZValidation[Nothing, String, RequestId] =
    s match {
      case id if id.startsWith("req-") => Validation.succeed(id: RequestId)
      case _ => Validation.fail("requestId must start with req-")
    }
}
```

---

# Validated Builder for Incoming Tool Call

```scala
final case class IncomingToolCall(
  requestId: RequestId,
  toolName: ToolName,
  argumentsJson: String
)

object IncomingToolCall {
  val validator = ValidatedBuilderGenerator.builder[IncomingToolCall]
}
```

```scala
val ok = IncomingToolCall.validator
  .requestId("req-001")
  .toolName("search")
  .argumentsJson("{\"query\":\"scala\"}")

val bad = IncomingToolCall.validator
  .requestId("001")
  .toolName("boom")
  .argumentsJson("{}")
```

---

# Strict vs Allow on Wrapped Inputs

```scala
final case class IncomingToolCall2(
  requestId: RequestId,
  toolName: ToolName,
  argumentsJson: String
)

object IncomingToolCall2 {
  val strict = ValidatedBuilderGenerator.builder[IncomingToolCall2]
  val allow = ValidatedBuilderGenerator.builderAllow[IncomingToolCall2]
}

val wrappedId = RequestId("req-002").toEither.toOption.get
val wrappedTool = ToolName("fetch").toOption.get

val accepted = IncomingToolCall2.allow
  .requestId(wrappedId)
  .toolName(wrappedTool)
  .argumentsJson("{}")
```

- strict: primitive input path with validation
- allow: can accept pre-wrapped values where representation permits

---

# Path-Aware Errors for Nested MCP Payloads

```scala
opaque type UserId = String
object UserId {
  def apply(s: String): Either[String, UserId] =
    if s.nonEmpty then Right(s: UserId)
    else Left("userId must be non-empty")
}

final case class IncomingMcpRequest(requestId: RequestId, userId: UserId)

object IncomingMcpRequest {
  val validator = ValidatedBuilderGenerator.builder[IncomingMcpRequest](
    ValidationPathConfig(customPrefix = Some("mcp.request"))
  )
}
```

```scala
val result = IncomingMcpRequest.validator
  .requestId("req-001")
  .userId("")

// failure path shape (illustrative):
// Seq(Custom("mcp.request"), Field("userId"))
```

Error points to where validation failed, not only what failed.

---

# Mixed Error Channels in MCP Domain

```scala
enum ErrorCode {
  case NotFound, Invalid
}

opaque type TenantId = String
object TenantId {
  def apply(s: String): ZValidation[Nothing, Throwable, TenantId] =
    s match {
      case "tenant-a" | "tenant-b" => Validation.succeed(s: TenantId)
      case other => Validation.fail(new IllegalArgumentException(s"Unknown tenant: $other"))
    }
}

opaque type ToolCode = String
object ToolCode {
  def apply(s: String): Either[ErrorCode, ToolCode] =
    if s.matches("[A-Z_]{3,}") then Right(s: ToolCode)
    else Left(ErrorCode.Invalid)
}

case class RoutedToolCall(requestId: RequestId, tenant: TenantId, code: ToolCode)
```

Union-like error unification is where validated builders reduce glue code.

---

# Why This Matters in MCP-like Flows

- request enters as untrusted payload
- domain wrappers guard invariants
- builders keep call sites readable
- path-aware errors improve debugging and observability

Readable failure surfaces reduce incident time.

---
layout: section
---

# 7) End-to-end contrast

---

# Without and With

<div class="two-col">
  <div class="panel">
    <div class="kicker">Without discipline</div>
    Constructor order mistakes
    Copy chains for nested updates
    Ad-hoc validation branches
  </div>
  <div class="panel">
    <div class="kicker">With this stack</div>
    ADTs for domain clarity
    Chimney/Ducktape for mapping
    Builders for readable construction
    Validated builders for typed input checks
  </div>
</div>

---

# Adoption Checklist

1. Start with ADT-first domain modeling.
2. Add mapping automation where layer translation grows.
3. Use simple builders when constructor readability drops.
4. Introduce validated builders at external boundaries.
5. Add optics when nested updates become repetitive.

---

# Q&A

Use appendix slides for implementation details and operational guidance.

---
layout: section
---

# Appendix A: Repository Anchors

---

# Builders APIs Used in This Deck

| Topic | Source |
|---|---|
| Simple builders intro | docs/readme.md |
| Simplest builder macro API | simple_builders/src/main/scala/api/BuilderGenerator.scala |
| Generic builder support | simple_builders/src/main/scala/api/BuilderGeneratorGeneric.scala |
| Validated builder macro API | with_zio_prelude/src/main/scala/api/ValidatedBuilderGenerator.scala |
| Smart constructors (`opaque`, `Either`, `ZValidation`) | with_zio_prelude/src/main/scala/playground/Opaque.scala |

---

# MCP Adaptation Note

MCP examples in this deck are domain-shaped, not copied from tests.

- They use the same builder and validator APIs from this repository.
- They keep the storyline practical for platform and API engineers.

---

# Appendix B: Simple vs Validated Builder Internals

`BuilderGeneratorSimplest`

- compile-time field completeness
- fluent named setters
- returns final model directly

`ValidatedBuilderGenerator`

- same fluent surface
- each field may invoke smart constructors
- returns `ZValidation` for accumulation and type-safe failures

Conceptually: construction pipeline vs validation-enriched pipeline.

---

# Appendix C: Strict vs Allow Decision Guide

Use `builder` / strict mode when:

- all external inputs should always pass through validation
- you do not want wrapped values to bypass field-level checks

Use `builderAllow` when:

- integration layers may already hold wrapped values
- you still want primitive inputs supported ergonomically

Team rule suggestion:

- strict at external boundaries
- allow in internal orchestration where wrappers are already trusted

---

# Appendix D: Validation Observability Schema

```scala
final case class ValidationEvent(
  requestId: String,
  toolName: String,
  errorPath: List[String],
  errorType: String,
  message: String,
  timestamp: Long
)
```

Recommended logging dimensions:

- `requestId`, `toolName`, `tenant`
- normalized `errorPath` (for aggregation)
- stable `errorType` (for alert routing)
- raw `message` (for debugging)

This turns path-aware failures into actionable operational signals.

---
layout: end
---

# Thank You

Questions and discussion.
