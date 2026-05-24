package com.github.aborg0.builders.example.simple

opaque type Region = String

object Region {
  def apply(raw: String): Either[String, Region] =
    raw match {
      case "US" => Right(raw: Region)
      case "EU" => Right(raw: Region)
      case _ => Left(s"Invalid region: $raw")
    }
}