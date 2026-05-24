package com.github.aborg0.builders.example.validating

import builders.configuration.GenerateBuilder
import builders.configuration.BuilderStyle
import com.github.aborg0.builders.example.simple.Simple
import com.github.aborg0.builders.example.simple.Gender
import com.github.aborg0.builders.example.simple.AcceptableDate
import com.github.aborg0.builders.example.simple.Region

@GenerateBuilder(style = BuilderStyle.Validating)
final case class VariousValid private(simple: Simple, gender: Gender, region: Region, tags: List[String], date: AcceptableDate)
