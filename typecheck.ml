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
      let t = normal_typecheck_term ~depth:(depth+1) gamma delta_or_P in
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
  dbg_print ~depth (Printf.sprintf "typecheck_term %s |- %s : ???\n" (string_of_term_t gamma_arg) (string_of_term_t m_arg));
  dbg_print ~depth (Printf.sprintf "%s\n" (m_arg |> Ast.sexp_of_term_t |> Sexp.to_string));

  match gamma_arg, m_arg with
  | gamma, Product (x, p, n) ->
    (* Product Formation (2) *)
    dbg_print ~depth "Applying Product Formation (2)\n";
    let res = normal_typecheck_term ~depth:(depth+1) (context_append gamma (x, p)) n in
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
    let q = normal_typecheck_term (context_append gamma (x, p)) n ~depth:(depth+1) in
    Product (x, p, q)
  | gamma, App (m, n) -> 
    (* Application *)
    dbg_print ~depth "Applying Application\n";
    begin match normal_typecheck_term gamma m ~depth:(depth+1) with
      | Product (x, p, q) ->
        let p' = normal_typecheck_term gamma n in
        m_assert (alpha_equiv p p') ~depth ~msg:(Printf.sprintf "Expected %s = %s\n" (string_of_term_t p) (string_of_term_t p'));
        subst_term q n x
      | typeof_m ->
        dbg_failwith ~depth (Printf.sprintf "(1) could not typecheck_term %s |- %s ... Found %s |- %s : %s where type is not a product\n"
                               (string_of_term_t gamma_arg) (string_of_term_t m_arg) (string_of_term_t gamma) (string_of_term_t m) (string_of_term_t typeof_m))
    end
  | _ ->
    dbg_failwith ~depth (Printf.sprintf "(2) could not typecheck_term %s |- %s\n" (string_of_term_t gamma_arg) (string_of_term_t m_arg))

and normal_typecheck_term ?(depth = 1) (gamma_arg : term_t) (m_arg : term_t) : term_t =
  (* typecheck_term ~depth gamma_arg m_arg *)
  (* let p = typecheck_term ~depth gamma_arg m_arg in
   * try
   *   let q = beta_equiv ~depth gamma_arg p in
   *   q
   * with _ ->
   *   p *)
  let p = typecheck_term ~depth gamma_arg m_arg in
  let q = beta_equiv_big_step ~depth gamma_arg p in
  q

and beta_equiv ?(depth = 1) (gamma : term_t) (m_arg : term_t) : term_t =
  (* TODO: Looks like beta equivalence is broken. For some reason,
  doing an application kinda fails *)
  dbg_print ~depth (Printf.sprintf "beta_equiv %s |- %s =~ ???\n"
                      (string_of_term_t gamma) (string_of_term_t m_arg));

  try (
    match m_arg with
    | App (Lambda (x, a, m), n) ->
      dbg_print ~depth "Applying Rule (1)\n";
      let _ = normal_typecheck_term (context_append gamma (x, a)) m ~depth:(depth+1) in
      let a' = normal_typecheck_term gamma n ~depth:(depth+1) in
      assert (alpha_equiv a a');
      subst_term m n x
    | App (m, n) ->
      dbg_print ~depth "Applying Rule (2)\n";
      let _ = normal_typecheck_term gamma m_arg ~depth:(depth+1) in
      let m1 = beta_equiv gamma m ~depth:(depth+1) in
      let n1 = beta_equiv gamma n ~depth:(depth+1) in
      App (m1, n1)
    | Lambda (x, p1, m1) ->
      dbg_print ~depth "Applying Rule (3)\n";
      let p2 = beta_equiv gamma p1 ~depth:(depth+1) in
      let m2 = beta_equiv (context_append gamma (x, p1)) m1 ~depth:(depth+1) in
      let _ = normal_typecheck_term (context_append gamma (x, p1)) m1 ~depth:(depth+1) in
      Lambda (x, p2, m2)
    | Product (x, p1, m1) ->
      dbg_print ~depth "Applying Rule (4)\n";
      let p2 = beta_equiv gamma p1 ~depth:(depth+1) in
      let m2 = beta_equiv (context_append gamma (x, p1)) m1 ~depth:(depth+1) in
      Product (x, p2, m2)
    | m ->
      dbg_print ~depth "Applying Rule (5)\n";
      let _ = normal_typecheck_term gamma m ~depth:(depth+1) in
      m
  ) with _ -> m_arg

and beta_equiv_big_step ?(depth = 1) (gamma : term_t) (m_arg : term_t) : term_t =
  let reduction = beta_equiv gamma m_arg ~depth in
  if (alpha_equiv m_arg reduction) then
    reduction
  else
    beta_equiv_big_step gamma reduction ~depth

let typecheck_single_term ?(is_final = false) term =
  printf "typecheck_single_term %s\n" (string_of_term_t term);
  if is_context term then (
    typecheck_context term Star;
    printf "Type: %s\n" (string_of_term_t Star);
    let normal_term = beta_equiv_big_step Star term in
    printf "Value: %s\n" (string_of_term_t normal_term);
    Star, normal_term
  ) else (
    let t = normal_typecheck_term Star term in
    printf "Type: %s\n" (string_of_term_t t);
    (* try *)
    let normal_term = beta_equiv_big_step Star term in
    if is_final then (
      printf "Value: %s\n" (string_of_term_t normal_term)
      (* with _ ->
       *   printf "Value: %s\n" (string_of_term_t term) *)
    );
    t, normal_term
  )

let rec typecheck_program p =
  match p with
  | Term term -> typecheck_single_term term ~is_final:true
  | Let (Untyped, x, t, p) ->
    print_endline "Untyped";
    typecheck_program (subst_binding x t p)
  | Let (Typed, x, t, p) ->
    print_string (sprintf "{%s} " x);
    let _, value = typecheck_single_term t in
    typecheck_program (subst_binding x value p)
  | Theorem (x, theorem, proof, prog) ->
    let _, value1 = typecheck_single_term theorem in
    let typ2, value2 = typecheck_single_term proof in
    (* print_endline "Checking if value for theorem matches type of proof";
     * print_endline (string_of_term_t value1);
     * print_endline (string_of_term_t typ2); *)
    assert (alpha_equiv value1 typ2);
    typecheck_program (subst_binding x value2 prog)

