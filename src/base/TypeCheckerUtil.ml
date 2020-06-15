(*
  This file is part of scilla.

  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.

  scilla is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  scilla.  If not, see <http://www.gnu.org/licenses/>.
*)

open Core_kernel
open! Int.Replace_polymorphic_compare
open Printf
open ParserUtil
open RecursionPrinciples
open DebugMessage
open MonadUtil
open Result.Let_syntax
open PatternChecker
open GasUseAnalysis
open TypeInfo
module PSRep = ParserRep
module PERep = ParserRep
module Parser = ScillaParser.Make (ParserSyntax)
module TC = TypeChecker.ScillaTypechecker (PSRep) (PERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep
module PM_Checker = ScillaPatternchecker (TCSRep) (TCERep)
module TI = ScillaTypeInfo (TCSRep) (TCERep)
module GUA_Checker = ScillaGUA (TCSRep) (TCERep)


(* Check that the expression parses *)
let check_parsing filename =
  match FrontEndParser.parse_file Parser.Incremental.exp_term filename with
  | Error _ -> fail0 (sprintf "Failed to parse input file %s\n." filename)
  | Ok e ->
      plog
      @@ sprintf "\n[Parsing]:\nExpression in [%s] is successfully parsed.\n"
           filename;
      pure e

(* Type check the expression with external libraries *)
let check_typing e elibs gas =
  let open TC in
  let open TC.TypeEnv in
  let rec_lib =
    {
      ParserSyntax.lname = TCIdentifier.mk_loc_id "rec_lib";
      ParserSyntax.lentries = recursion_principles;
    }
  in
  let tenv0 = TEnv.mk () in
  let%bind _typed_rec_libs, remaining_gas = type_library tenv0 rec_lib gas in
  (* Step 1: Type check external libraries *)
  let%bind _, remaining_gas = type_libraries elibs tenv0 remaining_gas in
  let%bind typed_e, remaining_gas =
    type_expr e tenv0 init_gas_kont remaining_gas
  in
  pure @@ (typed_e, remaining_gas)

let check_patterns e = PM_Checker.pm_check_expr e

let analyze_gas e = GUA_Checker.gua_expr_wrapper e
