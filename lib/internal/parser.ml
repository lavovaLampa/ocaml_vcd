open Lexer

exception
  ParseError of { expected : string; got : string; position : Lexing.position }

type scope = { scope_type : Lexer.scope; identifier : string } [@@deriving show]
type timescale = { value : int; unit : time_unit } [@@deriving show]
type 'a parse_result = Ok of 'a | EndOfFile | Err of Lexing.position

type var = {
  var_type : var_type;
  size : int;
  identifier : string;
  reference : reference;
}
[@@deriving show]

type declaration_cmd =
  | Comment of string
  | Date of string
  | Version of string
  | Scope of scope
  | Timescale of timescale
  | Upscope
  | Var of var
  | EndDefinitions
[@@deriving show]

let pp_array pp_item fmt items =
  Ppx_show_runtime.pp_list pp_item fmt (Array.to_list items)

type 'a value_change_dict = { identifier : string; value : 'a } [@@deriving show]

type value_change =
  | Scalar of char value_change_dict
  | BinaryVector of string value_change_dict
  | RealVector of string value_change_dict
  [@@deriving show]

(* let pp_value_change f v = *)
  (* match v with *)
  (* | Scalar { value; identifier } -> *)
      (* Format.fprintf f "%s%s" (show_binary_value value) identifier *)
  (* | BinaryVector { value; identifier } -> *)
      (* Format.fprintf f "b%s %s" *)
        (* ((String.concat "" % List.map show_binary_value % Array.to_list) value) *)
        (* identifier *)
  (* | RealVector v -> Format.fprintf f "%s" v *)

(* let show_value_change = Format.asprintf "%a" pp_value_change *)

type simulation_cmd =
  | Comment of string
  | DumpAll of value_change list
  | DumpOff of value_change list
  | DumpOn of value_change list
  | DumpVars of value_change list
  | SimulationTime of int
  | ValueChange of value_change
[@@deriving show]

type vcd_ast = {
  declarations : declaration_cmd list;
  simulation : simulation_cmd list;
}
[@@deriving show]

let next_declaration_cmd lexbuf : declaration_cmd parse_result =
  match Lexer.declaration lexbuf with
  | Comment t -> Ok (Comment t)
  | Date t -> Ok (Date t)
  | Version t -> Ok (Version t)
  | EndDefinitions -> Ok EndDefinitions
  | Scope { scope_type; identifier } -> Ok (Scope { scope_type; identifier })
  | Timescale { value; unit } -> Ok (Timescale { value; unit })
  | Upscope -> Ok Upscope
  | Var { var_type; size; identifier; reference } ->
      Ok (Var { var_type; size; identifier; reference })
  | EOF -> EndOfFile
  | _ -> failwith "Unexpected token!"

let seq_of_declaration lexbuf =
  Seq.unfold
    (fun lexbuf ->
      match next_declaration_cmd lexbuf with
      | EndOfFile -> None
      | Err x -> raise (ParseError { expected = ""; got = ""; position = x })
      | Ok EndDefinitions -> None
      | Ok x -> Some (x, lexbuf))
    lexbuf

let next_simulation_cmd lexbuf : simulation_cmd parse_result =
  match Lexer.simulation lexbuf with
  | Comment t -> Ok (Comment t)
  | DumpAll -> Ok (DumpAll []) (* TODO: Implement *)
  | DumpOff -> Ok (DumpOff []) (* TODO: Implement *)
  | DumpOn -> Ok (DumpOn []) (* TODO: Implement *)
  | DumpVars -> Ok (DumpVars []) (* TODO: Implement *)
  | SimulationTime t -> Ok (SimulationTime t)
  | ScalarValue { identifier; value } ->
      Ok (ValueChange (Scalar { value; identifier }))
  | BinaryVector { identifier; value } ->
      Ok (ValueChange (BinaryVector { value; identifier }))
  | RealVector { identifier; value } ->
      Ok (ValueChange (RealVector { value; identifier }))
  | EOF -> EndOfFile
  | _ -> failwith "Unexpected token!"

let seq_of_simulation lexbuf =
  Seq.unfold
    (fun lexbuf ->
      match next_simulation_cmd lexbuf with
      | EndOfFile -> None
      | Err x -> raise (ParseError { expected = ""; got = ""; position = x })
      | Ok x -> Some (x, lexbuf))
    lexbuf

let parse_ast lexbuf =
  let declarations = lexbuf |> seq_of_declaration |> List.of_seq in
  let simulation = lexbuf |> seq_of_simulation |> List.of_seq in
  { declarations; simulation }
