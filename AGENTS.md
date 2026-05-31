This project's goal is to provide helpers to generate typesafe builders for case classes. The main motivation is to avoid the boilerplate of writing builders by hand, while still providing a type-safe API that can be used in a fluent way.
This should improve the readability of code.

Prefer using the Metals MCP tools for intermediate steps, use sbt for final validations.
The main sbt tasks that you can use are:
- `--no-colors docs/mdoc` to generate the documentation with evaluated code snippets (this is required for the documentation to be generated in the `builders_docs` subproject)
- `--no-colors +test` to run tests across all supported Scala versions
  - `--no-colors withPrelude/test` to run tests with the ZIO prelude

In the code do not use indentation based constructs.