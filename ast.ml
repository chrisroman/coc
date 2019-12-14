open Core
open Common

type id = string [@@deriving sexp]
type term_t =
  | Id of id
  | Lambda of (id * term_t * term_t)
  | App of (term_t * term_t)
  | Product of (id * term_t * term_t)
  | Star
  [@@deriving sexp]

let rec string_of_term_t t =
  match t with
  | Id id ->
    id
  | Lambda (x, m, n) ->
    Printf.sprintf "(λ%s: %s)%s" x (string_of_term_t m) (string_of_term_t n)
  | App (m, n) -> 
    Printf.sprintf "(%s %s)" (string_of_term_t m) (string_of_term_t n)
  | Product (x, m, n) ->
    Printf.sprintf "[%s: %s]%s" x (string_of_term_t m) (string_of_term_t n)
  | Star ->
    "*"

let rec is_context (term : term_t) : bool = 
  match term with
  | Star -> true
  | Product (_, _, p) -> is_context p
  | _ -> false

let assert_context (term : term_t) =
  m_assert (is_context term)
    ~msg:(Printf.sprintf
            "Expected term %s to be a context\n" (string_of_term_t term))

(* If term = [x1:M1]...[xn:Mn]* then we split off just the last element, i.e.
 * components, i.e. return [x1:M1]...[xn-1:Mn-1]* and (xn, Mn)*
 * If term = [x:M]* then just return Star and [x:M]* *)
let rec context_split (term : term_t) : term_t * (id * term_t) =
  assert_context term;
  match term with
  | Product (x, m, Star) ->
    Star, (x, m)
  | Product (x, m, (Product (_, _, _) as p)) ->
    let (hd, tl) = context_split p in
    Product (x, m, hd), tl
  | _ -> failwith (Printf.sprintf
                     "Tried to call [context_tail] on an empty or invalid context term %s\n"
                     (string_of_term_t term)
                  )
