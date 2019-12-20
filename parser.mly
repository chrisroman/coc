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
%token LET
%token EQUALS
%token IN
%token UNTYPED
%token THEOREM
%token WITH
%token PROOF
%token SEMICOLON
%token EOF

%start <Ast.program option> start
%%

(* TODO: Add -> syntax sugar? *)
start:
  | EOF
  { None }
  | p = program
  { Some p }

program:
  | LET; UNTYPED; x = ID; EQUALS; t1 = term; IN; p = program
    { Let (Untyped, x, t1, p) }
  | LET; x = ID; EQUALS; t1 = term; IN; p = program
    { Let (Typed, x, t1, p) }
  | THEOREM; x = ID; EQUALS; t = term; WITH; PROOF; proof = term; SEMICOLON; p = program
    { Theorem (x, t, proof, p) }
  | term = term
  { Term term }
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
