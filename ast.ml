type id = Core.String.t [@@deriving sexp]
type term_t =
  | Id of id
  | Lambda of (id * term_t * term_t)
  | App of (term_t * term_t)
  | Product of (id * term_t * term_t)
  | Star
  [@@deriving sexp]
