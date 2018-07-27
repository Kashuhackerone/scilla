(*
 * Copyright (c) 2018 - present. 
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Core
open Big_int
open Syntax

val subst_type_in_type: string -> typ -> typ -> typ
val subst_type_in_literal: 'a ident -> typ -> literal -> literal
val subst_type_in_expr: 'a ident -> typ -> 'a expr -> 'a expr

(* An inferred type with possible qualifiers *)
type 'rep inferred_type = {
  tp   : typ;
  qual : 'rep
} [@@deriving sexp]

(* Qualifiers to type inference with additional information *)
module type QualifiedTypes = sig
  type t
  val mk_qualified_type : typ -> t inferred_type      
end

module type MakeTEnvFunctor = functor (Q: QualifiedTypes) -> sig
  (* Resolving results *)
  type resolve_result
  val rr_loc : resolve_result -> loc
  val rr_typ : resolve_result -> Q.t inferred_type
  val rr_pp  : resolve_result -> string
  val mk_qual_tp : typ -> Q.t inferred_type
  
  module TEnv : sig
    type t
    (* Make new type environment *)
    val mk : t
    (* Add to type environment *)
    val addT : t -> loc ident -> typ -> t
    (* Add to many type bindings *)
    val addTs : t -> (loc ident * typ) list -> t
    (* Add type variable to the environment *)
    val addV : t -> loc ident -> t
    (* Check type for well-formedness in the type environment *)
    val is_wf_type : t -> typ -> (unit, string) result
    (* Resolve the identifier *)    
    val resolveT : ?lopt:(loc option) -> t -> string ->
      (resolve_result, string) result
    (* Copy the environment *)
    val copy : t -> t
    (* Convert to list *)
    val to_list : t -> (string * resolve_result) list
    (* Get type variables *)
    val tvars : t -> (string * loc) list
    (* Print the type environment *)
    val pp : ?f:(string * resolve_result -> bool) -> t -> string        
  end
end

module PlainTypes : QualifiedTypes
module MakeTEnv : MakeTEnvFunctor

val literal_type : literal -> (typ, string) result

(* Useful generic types *)
val fun_typ : typ -> typ -> typ
val tvar : string -> typ
val tfun_typ : string -> typ -> typ
val map_typ : typ -> typ -> typ

(****************************************************************)
(*                       Type sanitization                      *)
(****************************************************************)

val is_sendable_type : typ -> bool

(****************************************************************)
(*             Utility function for matching types              *)
(****************************************************************)

val type_equiv : typ -> typ -> bool

val assert_type_equiv : typ -> typ -> (unit, string) result

(* Applying a function type *)
val fun_type_applies : typ -> typ list -> (typ, string) result

(* Applying a type function *)
val elab_tfun_with_args : typ -> typ list -> (typ, string) result

val pp_typ_list : typ list -> string  

(****************************************************************)
(*                        Working with ADTs                     *)
(****************************************************************)

(*  Apply type substitution  *)
val apply_type_subst : (string * typ) list -> typ -> typ

(*  Get elaborated type for a constructor and list of type arguments *)    
val elab_constr_type : string -> typ list -> (typ, string) result  

(* For a given instantiated ADT and a construtor name, get type *
   assignemnts. This is the main working horse of type-checking
   pattern-matching. *)    
val constr_pattern_arg_types : typ -> string -> typ -> (typ list, string) result  

val validate_param_length : string -> int -> int -> (unit, string) result

val assert_all_same_type : typ list -> (unit, string) result

(****************************************************************)
(*                  Better error reporting                      *)
(****************************************************************)

val wrap_with_info : string -> ('a, string) result -> ('a, string) result

val wrap_err : loc expr -> ?opt:string -> ('a, string) result -> ('a, string) result

val wrap_serr : loc stmt -> ?opt:string ->
  ('a, string) result -> ('a, string) result

(****************************************************************)
(*                  Built-in typed entities                     *)
(****************************************************************)

val blocknum_name : string
