open Core
open Ast
open Common

(* Implement Γ ⊢ Δ
 * For debugging purposes, keep track of the depth of recursion
 *)
let rec typecheck_context ?(depth = 1) (gamma : term_t) (delta : term_t) : unit =
  dbg_print ~depth (Printf.sprintf "typecheck_context %s |- %s\n" (string_of_term_t gamma) (string_of_term_t delta));

  assert_context gamma;
  assert_context delta;
  begin match gamma with
    (* Valid Contexts (1) *)
    | Star ->
      dbg_print ~depth "Applying Valid Contexts (1)\n";
      m_assert (delta = Star) ~depth ~msg:(Printf.sprintf "expected delta to be * but found %s\n" (string_of_term_t delta));

    | Product _ ->
      let gamma_body, (_x, delta_or_P) = context_split_bf gamma in
      (* TODO: It's very likely that we're allowed to try all typing rules *)
      (try
         (* Valid Contexts (2) *)
        dbg_print ~depth "Applying Valid Contexts (2)\n";
         m_assert (delta = Star) ~depth ~msg:(Printf.sprintf "expected delta to be * but found %s\n" (string_of_term_t delta));
         typecheck_context ~depth:(depth+1) gamma_body delta_or_P
       with _ ->
         (try
            (* Valid Contexts (3) *)
            dbg_print ~depth "Applying Valid Contexts (3)\n";
            typecheck_term ~depth:(depth+1) gamma_body delta_or_P Star
          with _ ->
            (try
               (* Product Formation (1) *)
               dbg_print ~depth "Applying Product Formation (1)\n";
               let (x, m), delta_tl = context_split_ht delta in
               typecheck_context ~depth:(depth+1) (context_append gamma (x, m)) delta_tl
             with _ ->
                  dbg_failwith ~depth (Printf.sprintf "could not typecheck_context %s |- %s\n" (string_of_term_t gamma) (string_of_term_t delta))
               )))

    (* Variables, Abstraction, and Application *)
    | _ -> dbg_failwith ~depth (Printf.sprintf "could not typecheck_context %s |- %s\n" (string_of_term_t gamma) (string_of_term_t delta))
  end

(* Implement Γ ⊢ M : P
 * For debugging purposes, keep track of the depth of recursion
 *)
and typecheck_term ?(depth = 1) (gamma : term_t) (m : term_t) (p : term_t) =
  dbg_print ~depth (Printf.sprintf "typecheck_term %s |- %s : %s\n" (string_of_term_t gamma) (string_of_term_t m) (string_of_term_t p));

  (try
     (* Product Formation (2) *)
     dbg_print ~depth "Applying Product Formation (2)\n";
     let (m_x, m_p), n = context_split_ht m in
     m_assert (p = Star) ~depth ~msg:(Printf.sprintf "expected P to be * but found %s\n" (string_of_term_t p));
     typecheck_term ~depth:(depth+1) (context_append gamma (m_x, m_p)) n Star
   with _ ->
     (try
        (* Variables *)
        dbg_print ~depth "Applying Variables\n";
        begin match m with
          | Id x ->
            if context_contains gamma (x, p) then
              typecheck_context ~depth:(depth+1) gamma Star
            else
              dbg_failwith ~depth (Printf.sprintf "Could not find %s in gamma=%s\n" (string_of_term_t m) (string_of_term_t gamma))
          | _ -> dbg_failwith ~depth (Printf.sprintf "expected m to be id _ but found %s\n" (string_of_term_t m));
        end
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
              dbg_failwith ~depth (Printf.sprintf "could not typecheck_term %s |- %s : %s\n" (string_of_term_t gamma) (string_of_term_t m) (string_of_term_t p))
           ))))

