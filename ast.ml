open Core
open Common

type id = string [@@deriving sexp]
type typed =
  | Untyped
  | Typed
type term_t =
  | Id of id
  | Lambda of (id * term_t * term_t)
  | App of (term_t * term_t)
  | Product of (id * term_t * term_t)
  | Star
  [@@deriving sexp]
type program =
  | Let of (typed * id * term_t * program)
  | Term of term_t
  | Theorem of (id * term_t * term_t * program)

let rec string_of_term_t t =
  match t with
  | Id id ->
    id
  | Lambda (x, m, n) ->
    Printf.sprintf "(λ%s: %s) %s" x (string_of_term_t m) (string_of_term_t n)
  | App (m, n) -> 
    Printf.sprintf "(%s %s)" (string_of_term_t m) (string_of_term_t n)
  | Product (x, m, n) ->
    Printf.sprintf "[%s: %s] %s" x (string_of_term_t m) (string_of_term_t n)
  | Star ->
    "*"

let string_of_typed tc =
  match tc with
  | Untyped -> " Untyped"
  | Typed -> ""

let rec string_of_program p =
  match p with
  | Let (tc, x, t, p') ->
    Printf.sprintf "let%s %s = %s in \n%s"
      (string_of_typed tc) x (string_of_term_t t) (string_of_program p')
  | Term t -> string_of_term_t t
  | Theorem (x, theorem, proof, prog) ->
    Printf.sprintf "Theorem %s = %s with \n\tProof %s;\n%s"
      x (string_of_term_t theorem) (string_of_term_t proof) (string_of_program prog)
 
let rec is_context (term : term_t) : bool = 
  match term with
  | Star -> true
  | Product (_, _, n) -> is_context n
  | _ -> false

let assert_context (term : term_t) =
  m_assert (is_context term)
    ~msg:(Printf.sprintf
            "Expected term %s to be a context\n" (string_of_term_t term))

(* If term = [x1:M1]...[xn:Mn]* then we split off just the last element, i.e.
 * components, i.e. return [x1:M1]...[xn-1:Mn-1]* and (xn, Mn)
 * If term = [x:M]* then just return * and (x, M)
 * For consistent naming, we'll say we return a tuple of the "body" and "foot"
 * of the list *)
let rec context_split_bf (term : term_t) : term_t * (id * term_t) =
  assert_context term;
  match term with
  | Product (x, m, Star) ->
    Star, (x, m)
  | Product (x, m, n) ->
    let (hd, tl) = context_split_bf n in
    Product (x, m, hd), tl
  | _ -> failwith (Printf.sprintf
                     "Tried to call [context_split_bf] on an empty or invalid context term %s\n"
                     (string_of_term_t term)
                  )

(* If term = [x1:M1]...[xn:Mn]* then return the head and tail
 * i.e. return (x1, m1) and [x2:M2]...[xn:Mn]*
 * If term = [x:M]* then just return (x, M) and * *)
let context_split_ht (term : term_t) : (id * term_t) * term_t =
  assert_context term;
  match term with
  | Product (x, m, Star) ->
    (x, m), Star
  | Product (x, m, n) ->
    (x, m), n
  | _ -> failwith (Printf.sprintf
                     "Tried to call [context_split_ht] on an empty or invalid context term %s\n"
                     (string_of_term_t term)
                  )

let rec context_append (context : term_t) (x, m : id * term_t) : term_t =
  assert_context context;
  match context with
  | Star -> Product (x, m, Star)
  | Product (x', m', n) -> Product (x', m', context_append n (x, m))
  | _ -> failwith (Printf.sprintf
                     "Tried to call [context_append] on an invalid context term %s\n"
                     (string_of_term_t context)
                  )

let rec context_contains (context : term_t) (x, p : id * term_t) : bool =
  assert_context context;
  match context with
  | Star -> false
  | Product (x', m', n) -> (x = x' && p = m') || (context_contains n (x, p))
  | _ -> failwith (Printf.sprintf
                     "Tried to call [context_contains] on an invalid context term %s\n"
                     (string_of_term_t context)
                  )

(* Allows shadowing *)
let rec context_get_impl (context : term_t) (x : id) (res : term_t option): term_t =
  match context with
  | Star -> 
    begin match res with
      | None -> failwith (Printf.sprintf "Couldn't find %s in %s" x (string_of_term_t context))
      | Some r -> r
    end
  | Product (x', m, n) ->
    let res = if x = x' then Some m else res in
    context_get_impl n x res
  | _ -> failwith (Printf.sprintf
                     "Tried to call [context_get_impl] on an invalid context term %s\n"
                     (string_of_term_t context)
                  )

let context_get (context : term_t) (x : id) : term_t =
  assert_context context;
  context_get_impl context x None

(* Return [n/x]term *)
let rec subst_term (term : term_t) (n : term_t) (x : id) : term_t =
  match term with
  | Star -> Star
  | Id x' -> if x = x' then n else term
  | App (m', n') -> App (subst_term m' n x, subst_term n' n x)
  | Lambda (x', m', n') ->
    if x' <> x then
      Lambda (x', subst_term m' n x, subst_term n' n x)
    else
      failwith "trying to substitute a bound variable"
  | Product (x', m', n') ->
    Product (x', subst_term m' n x, subst_term n' n x)

let rec alpha_equiv ?(map = []) (t1 : term_t) (t2 : term_t) : bool =
  match t1, t2 with
  | Star, Star -> true
  | Id x, Id y ->
    (* TODO: Not sure if the things aren't mapped, then they should be equal *)
    begin match List.Assoc.find map x ~equal:(=) with
      | None -> x = y
      | Some x' -> x' = y
    end
  | App (m1, n1), App (m2, n2) ->
    (alpha_equiv ~map m1 m2) && (alpha_equiv ~map n1 n2)
  | Lambda (x1, m1, n1), Lambda (x2, m2, n2)
  | Product (x1, m1, n1), Product (x2, m2, n2) ->
    let new_map = List.Assoc.add map x1 x2 ~equal:(=) in
    (alpha_equiv ~map:(new_map) m1 m2) && (alpha_equiv ~map:(new_map) n1 n2)
  | _ -> false

let rec subst_binding (x : id) (t : term_t) (p : program) : program =
  match p with
  | Term t' -> Term (subst_term t' t x)
  | Let (tc, x', t', p') -> Let (tc, x', (subst_term t' t x), (subst_binding x t p'))
  | Theorem (x', theorem, proof, prog) ->
    Theorem (x', (subst_term theorem t x), (subst_term proof t x), (subst_binding x t prog))
