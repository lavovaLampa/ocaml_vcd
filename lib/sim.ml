open Internal
open Vcd

let pp_array pp_item fmt items =
  Ppx_show_runtime.pp_list pp_item fmt (Array.to_list items)

type value_change =
  | Scalar of {
      identifier : string;
      var : Vcd.scoped_var list;
      value : Lexer.binary_value;
    }
  | BinaryVector of {
      identifier : string;
      var : Vcd.scoped_var list;
      value : Lexer.binary_value array;
    }
  | RealVector of {
      identifier : string;
      var : Vcd.scoped_var list;
      value : string;
    }
[@@deriving show]

(** Rules for left-extending vector values *)
let left_extend_value bin_value =
  match bin_value with Lexer.One -> Lexer.Zero | x -> x
(* Rules for '-' and 'U' are not defined, but we still extend them if necessary *)

(** [left_extend_vector size vec] left-extends [vec] to [size]*)
let left_extend_vector size vec =
  let vec_size = Array.length vec in
  assert (vec_size > 0);
  (* 0 sized vector does not make sense*)
  if vec_size = size then vec
  else
    let result = Array.make size (left_extend_value vec.(0)) in
    Array.blit vec 0 result (size - vec_size) vec_size;
    result

let preprocess_value_change vcd v =
  match v with
  | Parser.Scalar { value; identifier } ->
      let var = Vcd.var_of_identifier vcd identifier in
      Scalar { identifier; var; value }
  | Parser.BinaryVector { value; identifier } ->
      let var = Vcd.var_of_identifier vcd identifier in
      assert (List.length var > 0);
      let value = left_extend_vector (List.hd var).var.size value in
      BinaryVector { identifier; var; value }
  | Parser.RealVector s ->
      RealVector { identifier = "TODO"; var = []; value = s }
(* FIXME: Implement real vector *)

let collect_value_changes vcd seq =
  let seq_state = ref seq in
  let[@tail_mod_cons] rec aux seq =
    let open Seq in
    match seq () with
    | Nil ->
        seq_state := empty;
        []
    | Cons (v, next) -> (
        match v with
        | Parser.ValueChange v -> preprocess_value_change vcd v :: aux next
        | Parser.DumpVars v ->
            List.map (preprocess_value_change vcd) v @ aux next
        | Parser.SimulationTime _ ->
            seq_state := cons v next;
            []
        | _ -> aux next)
  in
  (!seq_state, aux seq)

let seq_of_sim ({ src = { src; sim_byte_offset }; _ } as vcd) =
  let lexbuf, close_buf =
    match src with
    | String s ->
        ( Sedlexing.Utf8.from_string
            (String.sub s sim_byte_offset (String.length s - sim_byte_offset)),
          Fun.id )
    | File f ->
        let ic = In_channel.open_bin f in
        In_channel.seek ic (Int64.of_int sim_byte_offset);
        (Sedlexing.Utf8.from_channel ic, fun () -> In_channel.close ic)
  in
  let rec group_by_timestamp seq () =
    let rec aux curr_time seq =
      let open Seq in
      match curr_time with
      | None -> (
          match seq () with
          | Nil ->
              close_buf ();
              Nil
          | Cons (v, next) -> (
              match v with
              | Parser.SimulationTime t -> aux (Some t) next
              | _ -> aux curr_time next))
      | Some t ->
          let next, values = collect_value_changes vcd seq in
          Cons ((t, values), group_by_timestamp next)
    in
    aux None seq
  in
  group_by_timestamp (Parser.seq_of_simulation lexbuf)
