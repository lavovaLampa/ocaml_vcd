open Ocaml_vcd

(* let rec show_tokens lexbuf is_sim =
   match Lexer.lex lexbuf with
   | None -> ()
   | Some x -> (
       match x with
       | Identifier idx ->
           if is_sim then
             print_endline
               (Lexer.show_identifier_subtype (Lexer.lex_sim_identifier idx))
           else print_endline (Lexer.show_token x);
           show_tokens lexbuf is_sim
       | Keyword EndDefinitions ->
           print_endline (Lexer.show_token x);
           show_tokens lexbuf true
       | _ ->
           print_endline (Lexer.show_token x);
           show_tokens lexbuf is_sim) *)

let () =
  let fjel = Sys.argv.(1) in
  let main ch =
    Printexc.register_printer Parser.parse_error_printer;
    let lexbuf = Sedlexing.Utf8.from_channel ch in
    (* show_tokens lexbuf false *)
    let result = Parser.parse_ast lexbuf in
    print_endline @@ Parser.show_simulation @@ List.hd @@ result.simulation;
    Printf.printf "DONE\n"
  in
  In_channel.with_open_text fjel main
