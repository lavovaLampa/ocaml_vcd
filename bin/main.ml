open Ocaml_vcd

(* open Ocaml_vcd.Internal *)
open! Ocaml_vcd.Util

let () =
  let fjel = Sys.argv.(1) in
  let id = Sys.argv.(2) in
  let main () =
    (* let lule f = *)
    (* let vcd = Vcd.from_utf8_string (In_channel.input_all f) in *)
    (* let sim_seq = Sim.seq_of_sim vcd in *)
    (* Seq.iter (fun _ -> ()) sim_seq *)
    (* in *)
    (* In_channel.with_open_text fjel lule; *)
    let vcd = Vcd.from_file fjel in
    let variables = Vcd.var_of_identifier vcd id in
    let version = Vcd.version vcd in
    let date = Vcd.date vcd in
    let timescale = Vcd.timescale vcd in
    if Option.is_some version then
      Printf.printf "Version: %s\n" @@ String.trim @@ Option.get version;
    Printf.printf "Date: %s\n" @@ String.trim @@ Option.get date;
    print_endline @@ Internal.Parser.show_timescale timescale;
    List.iter (print_endline % Vcd.string_of_var) variables;
    let sim_seq = Vcd.seq_of_simulation vcd in
    Seq.iter (print_endline % Internal.Parser.show_simulation_cmd) sim_seq
  in
  main ()
