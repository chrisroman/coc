%{
    open Ast
    (* open Common *)
    (* open Core *)

    (* let locations = ref [] *)


    (* let add (node : node) position =
      locations := List.Assoc.add !locations node (location_of_position position)
                                  ~equal:phys_equal *)
%}

%token <string> ID
%token LAMBDA
%token COLON
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token STAR
%token EOF

%start <Ast.term_t option> start
%%

(* TODO: Add -> syntax sugar? *)
start:
  | EOF
  { None }
  | term = term
  { Some term }
;

term:
  | id = ID
    { Id id }
  | LPAREN; LAMBDA; id = ID; COLON; m = term; RPAREN; n = term
    { Lambda (id, m, n) }
  | LPAREN; m = term; n = term; RPAREN
    { App (m, n) }
  | LBRACK; id = ID; COLON; m = term; RBRACK; n = term
    { Product (id, m, n) }
  | STAR
    { Star }
;
