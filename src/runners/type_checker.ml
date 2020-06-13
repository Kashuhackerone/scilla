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
open Scilla_base
open TypeUtil
open RunnerUtil
open DebugMessage
open PrettyPrinters
open ErrorUtils
open TypeCheckerUtil

let run () =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let cli = parse_cli None ~exe_name:Sys.argv.(0) in
  let open GlobalConfig in
  StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
  set_debug_level Debug_None;
  let filename = cli.input_file in
  let gas_limit = cli.gas_limit in
  match FrontEndParser.parse_file Parser.Incremental.exp_term filename with
  | Ok e -> (
      (* Get list of stdlib dirs. *)
      let lib_dirs = StdlibTracker.get_stdlib_dirs () in
      if List.is_empty lib_dirs then stdlib_not_found_err ();
      (* Import all libs. *)
      let std_lib = import_all_libs lib_dirs in
      match check_typing e std_lib gas_limit with
      | Ok (((_, (e_typ, _)) as typed_erep), _remaining_gas) -> (
          match check_patterns typed_erep with
          | Ok _ -> (
              let tj =
                [ ("type", `String (FrontEndParser.FEPType.pp_typ e_typ.tp)) ]
              in
              let output_j =
                `Assoc
                  ( if cli.p_type_info then
                    ( "type_info",
                      JSON.TypeInfo.type_info_to_json
                        (TI.type_info_expr typed_erep) )
                    :: tj
                  else tj )
              in
              pout (sprintf "%s\n" (Yojson.Basic.pretty_to_string output_j));
              if cli.gua_flag then
                match analyze_gas typed_erep with
                | Ok _ -> ()
                | Error el -> fatal_error el )
          | Error el -> fatal_error el )
      | Error ((_, el), _remaining_gas) -> fatal_error el )
  | Error e -> fatal_error e

let () = try run () with FatalError msg -> exit_with_error msg
