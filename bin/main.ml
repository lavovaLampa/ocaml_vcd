open Ocaml_vcd
open Ocaml_vcd.Internal
open Ocaml_vcd.Util

let () =
  let fjel = Sys.argv.(1) in
  let id = Sys.argv.(2) in
  let main () =
    Printexc.register_printer Parser.parse_error_printer;
    let vcd = Vcd.from_utf8_file fjel in
    let variables = Vcd.var_of_identifier vcd id in
    let version = Vcd.version vcd in
    let date = Vcd.date vcd in
    let timescale = Vcd.timescale vcd in
    Printf.printf "Version: %s\n" @@ String.trim @@ Option.get version;
    Printf.printf "Date: %s\n" @@ String.trim @@ Option.get date;
    print_endline @@ Parser.show_timescale timescale;
    List.iter (print_endline % Vcd.string_of_var) variables;
    let sim_seq = Sim.seq_of_sim vcd in
    match sim_seq () with
    | Nil -> ()
    | Seq.Cons ((_, values), _) -> List.iter (fun _ -> ()) values
  in
  main ()
