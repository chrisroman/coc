open Core
open Ast
open Common

(* Implement Γ ⊢ Δ
 * For debugging purposes, keep track of the depth of recursion
 *)
let rec typecheck_context ?(depth = 1) (gamma : term_t) (delta : term_t) : unit =
  dbg_print ~depth (Printf.sprintf
                      "typecheck_context %s |- %s\n"
                      (string_of_term_t gamma)
                      (string_of_term_t delta));

  assert_context gamma;
  assert_context delta;
  begin match gamma with
    (* Valid Contexts (1) *)
    | Star ->
      dbg_print ~depth "Applying Valid Contexts (1)\n";
      if delta = Star then
        ()
      else
        dbg_failwith ~depth (Printf.sprintf "Expected delta = *, found %s\n" (string_of_term_t delta))

    | Product _ ->
      let gamma_body, (_x, delta_or_P) = context_split_bf gamma in
      (* TODO: It's very likely that we're allowed to try all typing rules *)
      (try
         (* Valid Contexts (2) *)
        dbg_print ~depth "Applying Valid Contexts (2)\n";
         m_assert (delta = Star)
           ~depth
           ~msg:(Printf.sprintf "expected delta to be * but found %s\n"
                   (string_of_term_t delta));
         typecheck_context ~depth:(depth+1) gamma_body delta_or_P
       with _ ->
         (try
            (* Valid Contexts (3) *)
            dbg_print ~depth "Applying Valid Contexts (3)\n";
            typecheck_term ~depth:(depth+1) gamma delta_or_P Star
          with _ ->
            (try
               (* Product Formation (1) *)
               dbg_print ~depth "Applying Product Formation (1)\n";
               let (x, m), delta_tl = Ast.context_split_ht delta in
               typecheck_context ~depth:(depth+1) (context_append gamma (x, m)) delta_tl
             with _ ->
               (try
                  (* Product Formation (2) *)
                  dbg_print ~depth "Applying Product Formation (2)\n";
                  dbg_failwith ~depth "typecheck_context product formation (2) unimplemented"
                with _ ->
                  dbg_failwith ~depth (Printf.sprintf
                              "could not typecheck_context %s |- %s\n"
                              (string_of_term_t gamma)
                              (string_of_term_t delta))
               ))))

    (* Variables, Abstraction, and Application *)
    | _ -> dbg_failwith ~depth (Printf.sprintf
                       "could not typecheck_context %s |- %s\n"
                       (string_of_term_t gamma)
                       (string_of_term_t delta))
  end

(* Implement Γ ⊢ M : P
 * For debugging purposes, keep track of the depth of recursion
 *)
and typecheck_term ?(depth = 1) (gamma : term_t) (m : term_t) (p : term_t) =
  dbg_print ~depth (Printf.sprintf
                      "typecheck_term %s |- %s : %s\n"
                      (string_of_term_t gamma)
                      (string_of_term_t m)
                      (string_of_term_t p));

  (try
     (* Variables *)
     dbg_print ~depth "Applying Variables\n";
     dbg_failwith ~depth "typecheck_term variables unimplemented"
   with _ -> 
     (try
        (* Abstraction *)
        dbg_print ~depth "Applying Abstraction\n";
        dbg_failwith ~depth "typecheck_term abstraction unimplemented"
      with _ ->
        (try
           (* Application *)
           dbg_print ~depth "Applying Application\n";
           dbg_failwith ~depth "typecheck_term application unimplemented"
         with _ ->
           dbg_failwith ~depth (Printf.sprintf
                       "could not typecheck_term %s |- %s : %s\n"
                       (string_of_term_t gamma)
                       (string_of_term_t m)
                       (string_of_term_t p))
        )))

