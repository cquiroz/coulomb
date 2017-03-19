/*
Copyright 2017 Erik Erlandson

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.manyangled.coulomb

import scala.annotation.implicitNotFound
import scala.language.experimental.macros

case class ChurchIntValue[N <: ChurchInt](value: Int)

object ChurchIntValue {
  implicit def witnessChurchIntValue[N <: ChurchInt]: ChurchIntValue[N] =
    macro ChurchIntMacros.churchIntValue[N]
}

case class UnitRec[UE <: UnitExpr](name: String, coef: Double)

case class TempUnitRec[UE <: TemperatureExpr](offset: Double)

@implicitNotFound("Implicit not found: CompatUnits[${U1}, ${U2}].\nIncompatible Unit Expressions: ${U1} and ${U2}")
class CompatUnits[U1 <: UnitExpr, U2 <: UnitExpr](val coef: Double) {
}

object CompatUnits {
  implicit def witnessCompatUnits[U1 <: UnitExpr, U2 <: UnitExpr]: CompatUnits[U1, U2] =
    macro UnitMacros.compatUnits[U1, U2]
}

case class UnitExprString[U <: UnitExpr](str: String)

object UnitExprString {
  implicit def witnessUnitExprString[U <: UnitExpr]: UnitExprString[U] =
    macro UnitMacros.unitExprString[U]
}

trait UnitExprMul[U1 <: UnitExpr, U2 <: UnitExpr] {
  type U <: UnitExpr
  def coef: Double
}

object UnitExprMul {
  implicit def witnessUnitExprMul[U1 <: UnitExpr, U2 <: UnitExpr]: UnitExprMul[U1, U2] =
    macro UnitMacros.unitExprMul[U1, U2]
}

trait UnitExprDiv[U1 <: UnitExpr, U2 <: UnitExpr] {
  type U <: UnitExpr
  def coef: Double
}

object UnitExprDiv {
  implicit def witnessUnitExprDiv[U1 <: UnitExpr, U2 <: UnitExpr]: UnitExprDiv[U1, U2] =
    macro UnitMacros.unitExprDiv[U1, U2]
}