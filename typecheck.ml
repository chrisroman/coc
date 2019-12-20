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
    dbg_print ~depth "Applying Valid Contexts (1)\n"
  | Product _, Star ->
    let gamma, (_x, delta_or_P) = context_split_bf gamma_arg in
    if is_context delta_or_P then (
      (* Valid Contexts (2) *)
      dbg_print ~depth "Applying Valid Contexts (2)\n";
      typecheck_context ~depth:(depth+1) gamma delta_or_P;
    ) else (
      (* Valid Contexts (3) *)
      dbg_print ~depth "Applying Valid Contexts (3)\n";
      let t = typecheck_term ~depth:(depth+1) gamma delta_or_P in
      m_assert (t = Star) ~depth ~msg:(Printf.sprintf "(1) Expected to typecheck to * but found %s\n" (string_of_term_t t))
    )
  | gamma, Product (x, p, delta) ->
    (* Product Formation (1) *)
    dbg_print ~depth "Applying Product Formation (1)\n";
    typecheck_context ~depth:(depth+1) (context_append gamma (x, p)) delta
  | _ ->
    dbg_failwith ~depth (Printf.sprintf "could not typecheck_context %s \n" (string_of_term_t gamma_arg))

(* Implement Γ ⊢ M : P
 * For debugging purposes, keep track of the depth of recursion
 *)
and typecheck_term ?(depth = 1) (gamma_arg : term_t) (m_arg : term_t) : term_t =
  dbg_print ~depth (Printf.sprintf "typecheck_term %s |- %s\n" (string_of_term_t gamma_arg) (string_of_term_t m_arg));
  dbg_print ~depth (Printf.sprintf "%s\n" (m_arg |> Ast.sexp_of_term_t |> Sexp.to_string));

  match gamma_arg, m_arg with
  | gamma, Product (x, p, n) ->
    (* Product Formation (2) *)
    dbg_print ~depth "Applying Product Formation (2)\n";
    let res = typecheck_term ~depth:(depth+1) (context_append gamma (x, p)) n in
    m_assert (res = Star) ~depth ~msg:(Printf.sprintf "(2) Expected to typecheck to * but found %s\n" (string_of_term_t res));
    Star
  | gamma, Id x ->
    dbg_print ~depth "Applying Variables\n";
    let p = context_get gamma x in
    typecheck_context ~depth:(depth+1) gamma Star;
    p
  | gamma, Lambda (x, p, n) ->
    (* Abstraction *)
    dbg_print ~depth "Applying Abstraction\n";
    let q = typecheck_term (context_append gamma (x, p)) n ~depth:(depth+1) in
    Product (x, p, q)
  | gamma, App (m, n) -> 
    (* Application *)
    dbg_print ~depth "Applying Application\n";
    begin match typecheck_term gamma m ~depth:(depth+1) with
      | Product (x, p, q) ->
        let p' = typecheck_term gamma n in
        m_assert (alpha_equiv p p') ~depth ~msg:(Printf.sprintf "Expected %s = %s\n" (string_of_term_t p) (string_of_term_t p'));
        subst_term q n x
      | typeof_m ->
        dbg_failwith ~depth (Printf.sprintf "(1) could not typecheck_term %s |- %s ... Found %s |- %s : %s where type is not a product\n"
                               (string_of_term_t gamma_arg) (string_of_term_t m_arg) (string_of_term_t gamma) (string_of_term_t m) (string_of_term_t typeof_m))
    end
  | _ ->
    dbg_failwith ~depth (Printf.sprintf "(2) could not typecheck_term %s |- %s\n" (string_of_term_t gamma_arg) (string_of_term_t m_arg))


let typecheck_term term =
  if is_context term then (
    typecheck_context term Star;
    printf "Type: %s\n" (string_of_term_t Star)
  ) else (
    let t = typecheck_term Star term in
    printf "Type: %s\n" (string_of_term_t t)
  )

let rec typecheck_program p =
  match p with
  | Term term -> typecheck_term term
  | Let (x, t, p) ->
    typecheck_term t;
    typecheck_program (subst_binding x t p)
