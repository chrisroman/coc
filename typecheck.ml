open Core
open Ast
open Common

(* Implement Γ ⊢ Δ
 * For debugging purposes, keep track of the depth of recursion
 *)
let rec typecheck_context ?(depth = 1) (gamma_arg : term_t) (delta_arg : term_t) : unit =
  dbg_print ~depth (Printf.sprintf "typecheck_context %s |- %s\n" (string_of_term_t gamma_arg) (string_of_term_t delta_arg));
  assert_context gamma_arg;
  assert_context delta_arg;

  match gamma_arg, delta_arg with
  (* Valid Contexts (1) *)
  | Star, Star ->
    dbg_print ~depth "Applying Valid Contexts (1)\n";
  | Product _, Star ->
    let gamma, (_x, delta_or_P) = context_split_bf gamma_arg in
    if is_context delta_or_P then (
      (* Valid Contexts (2) *)
      dbg_print ~depth "Applying Valid Contexts (2)\n";
      typecheck_context ~depth:(depth+1) gamma delta_or_P
    ) else (
      (* Valid Contexts (3) *)
      dbg_print ~depth "Applying Valid Contexts (3)\n";
      typecheck_term ~depth:(depth+1) gamma delta_or_P Star
    )
  | gamma, Product (x, p, delta) ->
    (* Product Formation (1) *)
    dbg_print ~depth "Applying Product Formation (1)\n";
    typecheck_context ~depth:(depth+1) (context_append gamma (x, p)) delta
  | _ ->
    dbg_failwith ~depth (Printf.sprintf "could not typecheck_context %s |- %s\n" (string_of_term_t gamma_arg) (string_of_term_t delta_arg))

(* Implement Γ ⊢ M : P
 * For debugging purposes, keep track of the depth of recursion
 *)
and typecheck_term ?(depth = 1) (gamma_arg : term_t) (m_arg : term_t) (p_arg : term_t) =
  dbg_print ~depth (Printf.sprintf "typecheck_term %s |- %s : %s\n"
                      (string_of_term_t gamma_arg) (string_of_term_t m_arg) (string_of_term_t p_arg));

  match gamma_arg, m_arg, p_arg with
  | gamma, Product (x, p, n), Star ->
    (* Product Formation (2) *)
    dbg_print ~depth "Applying Product Formation (2)\n";
    typecheck_term ~depth:(depth+1) (context_append gamma (x, p)) n Star
  | gamma, Id x, p ->
    dbg_print ~depth "Applying Variables\n";
    if context_contains gamma (x, p) then
      typecheck_context ~depth:(depth+1) gamma Star
    else
      dbg_failwith ~depth (Printf.sprintf "Could not find %s in gamma=%s\n" (string_of_term_t (Id x)) (string_of_term_t gamma))
  | _gamma, Lambda (_x, _p, _n), Product (_x', _p', _q) ->
    (* Abstraction *)
    dbg_print ~depth "Applying Abstraction\n";
    dbg_failwith ~depth "typecheck_term abstraction unimplemented\n"
  | _gamma, App (_m, _n), _q' -> 
    (* Application *)
    dbg_print ~depth "Applying Application\n";
    dbg_failwith ~depth "typecheck_term application unimplemented\n"
  | _ ->
    dbg_failwith ~depth (Printf.sprintf "could not typecheck_term %s |- %s : %s\n" (string_of_term_t gamma_arg) (string_of_term_t m_arg) (string_of_term_t p_arg))


