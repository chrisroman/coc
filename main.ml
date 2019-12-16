[@@@part "0"] ;;
open Core
open Lexer
open Lexing
open Ast
open Common

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.start Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

[@@@part "1"] ;;

let rec parse_and_print lexbuf typecheck =
  match parse_with_error lexbuf with
  | Some term ->
    printf "%s\n" (Ast.string_of_term_t term);
    if typecheck then (
      if is_context term then (
        Typecheck.typecheck_context term Star;
        printf "Type: %s\n" (string_of_term_t Star)
      ) else (
        let t = Typecheck.typecheck_term Star term in
        printf "Type: %s\n" (string_of_term_t t)
      )
    );
    (* printf "%s\n" "Found a value!"; *)
    parse_and_print lexbuf typecheck
  | None -> ()

let loop filename typecheck () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf typecheck;
  In_channel.close inx

let () =
  let command =
    Command.basic
      ~summary:"Run a Calculus of Constructions program"
      Command.Let_syntax.(
        let%map_open
          filename = anon ("filename" %: string)
        and typecheck = flag "--typecheck" no_arg ~doc:" typecheck the program"
        and debug = flag "--debug" no_arg ~doc:" typecheck the program"
        in
        fun () ->
          config_debug := debug;
          loop filename typecheck ()
      )
  in
  Command.run command
