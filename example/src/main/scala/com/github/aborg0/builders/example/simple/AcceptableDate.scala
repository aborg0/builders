package com.github.aborg0.builders.example.simple

import java.time.LocalDate
import java.time.format.DateTimeParseException

opaque type AcceptableDate = LocalDate
object AcceptableDate {
  def apply(raw: String): Either[String, AcceptableDate] =
    try {
      Right(LocalDate.parse(raw)).filterOrElse(date => !date.isBefore(LocalDate.of(1900, 1, 1)), s"Date must be on or after 1900-01-01: $raw")
    } catch {
      case _: DateTimeParseException => Left(s"Invalid date format: $raw. Expected format: YYYY-MM-DD")
    }
}

