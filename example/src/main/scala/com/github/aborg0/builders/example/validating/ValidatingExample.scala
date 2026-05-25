package com.github.aborg0.builders.example.validating

import com.github.aborg0.builders.example.simple.Simple
import com.github.aborg0.builders.example.simple.Gender
import com.github.aborg0.builders.example.simple.AcceptableDate
import com.github.aborg0.builders.example.simple.Region

object ValidatingExample {
  def main(args: Array[String]): Unit = {
    // Example usage of the generated builder for VariousValid
    val result = VariousValid.builderNoAllow
      .simple(Simple.builder.field1("hello").field2(42).field3(true))
      .gender(Gender.Female)
      .region("US")
      .tags(List("tag1", "tag2"))
      .date("2024-01-01")
    println(result)
  }
}
