open Core

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
