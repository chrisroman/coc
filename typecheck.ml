open Core
open Ast
open Common

(* Implement Γ ⊢ Δ *)
let rec typecheck_context (gamma : term_t) (delta : term_t) : unit =
  assert_context gamma;
  assert_context delta;
  begin match gamma with
    (* Valid Contexts *)
    | Star ->
      if delta = Star then
        ()
      else
        failwith (Printf.sprintf "Expected *, found %s\n" (string_of_term_t delta))

    (* Product Formation *)
    | Product _ ->
      let gamma_hd, (_x, delta_or_P) = context_split gamma in
      (* TODO: It's very likely that we're allowed to use either typing rule *)
      begin try
          (* Case 1 *)
          m_assert (delta = Star)
            ~msg:(Printf.sprintf "expected delta to be * but found %s\n"
                    (string_of_term_t delta));
          typecheck_context gamma_hd delta_or_P
        with _ ->
          (* Case 2 *)
          typecheck_term gamma delta_or_P Star
      end

    (* Variables, Abstraction, and Application *)
    | _ -> failwith (Printf.sprintf
                       "could not typecheck_context %s\n"
                       (string_of_term_t gamma))
  end

(* Implement Γ ⊢ M : P *)
and typecheck_term (_gamma : term_t) (_m : term_t) (_p : term_t) =
  failwith "typecheck_term unimplemented"

