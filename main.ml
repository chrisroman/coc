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

let rec parse_and_print lexbuf typecheck =
  match parse_with_error lexbuf with
  | Some prog ->
    printf "%s\n" (Ast.string_of_program prog);
    if typecheck then (
      Typecheck.typecheck_program prog
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

let rec run_repl () =
  print_string "> ";
  match Out_channel.(flush stdout); In_channel.(input_line_exn stdin) with
  | exception End_of_file -> ()
  | cmd -> 
    (try 
       let lexbuf = Lexing.from_string cmd in
       parse_and_print lexbuf true
     with e -> print_endline (Exn.to_string e)
    );
    Out_channel.newline stdout;
    run_repl ()

let () =
  let command =
    Command.basic
      ~summary:"Run a Calculus of Constructions program"
      Command.Let_syntax.(
        let%map_open
          files = anon (sequence ("filename" %: string))
        and typecheck = flag "--typecheck" no_arg ~doc:" typecheck the program"
        and debug = flag "--debug" no_arg ~doc:" typecheck the program"
        and repl = flag "--repl" no_arg ~doc:" run in REPL mode"
        in
        fun () ->
          config_debug := debug;
          if repl then (
            print_endline "Welcome to the Calculus of Constructions! Type in your terms below";
            run_repl ()
          ) else (
            loop (List.hd_exn files) typecheck ()
          )
      )
  in
  Command.run command
