open Lexer
open Util

exception
  ParseError of { expected : string; got : string; position : Lexing.position }

type scope_type = Module | Task | Function | Begin | Fork [@@deriving show]

type time_unit =
  | Second
  | Millisecond
  | Microsecond
  | Nanosecond
  | Picosecond
  | Femtosecond
[@@deriving show]

type var_type =
  | Event
  | Integer
  | Parameter
  | Real
  | Reg
  | Supply0
  | Supply1
  | Time
  | Tri
  | TriAnd
  | TriOr
  | TriReg
  | Tri0
  | Tri1
  | WAnd
  | Wire
  | WOr
[@@deriving show]

type scope = { scope_type : scope_type; identifier : string } [@@deriving show]
type timescale = { number : int; time_unit : time_unit } [@@deriving show]

type var = {
  var_type : var_type;
  size : int;
  identifier : string;
  reference : Lexer.reference;
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

type value_change =
  | Scalar of { value : Lexer.binary_value; identifier : string }
  | BinaryVector of { value : Lexer.binary_value array; identifier : string }
  | RealVector of string

let pp_value_change f v =
  match v with
  | Scalar { value; identifier } ->
      Format.fprintf f "%s%s" (show_binary_value value) identifier
  | BinaryVector { value; identifier } ->
      Format.fprintf f "b%s %s"
        ((String.concat "" % List.map show_binary_value % Array.to_list) value)
        identifier
  | RealVector v -> Format.fprintf f "%s" v

let show_value_change = Format.asprintf "%a" pp_value_change

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

let parse_error_printer err =
  let to_string expected got
      ({ pos_lnum; pos_bol; pos_cnum; _ } : Lexing.position) =
    Printf.sprintf
      "Parser error.\n\tExpected: %s\n\tGot: %s\n\tLocation: %d, %d" expected
      got pos_lnum (pos_cnum - pos_bol)
  in
  match err with
  | ParseError { expected; got; position } ->
      Some (to_string expected got position)
  | _ -> None

let[@tail_mod_cons] parse_error lexbuf expected got =
  raise
    (ParseError
       { expected; got; position = fst (Sedlexing.lexing_positions lexbuf) })

let pop_token lexbuf token_type =
  match Lexer.lex_identifier lexbuf token_type with
  | exception EndOfFile -> parse_error lexbuf (show_token_type token_type) "EOF"
  | None ->
      parse_error lexbuf
        (show_token_type token_type)
        "" (* FIXME: Add better error handling *)
  | Some x -> x

let parse_end lexbuf =
  let _ = pop_token lexbuf End in
  ()

let parse_any_text lexbuf =
  let text = pop_token lexbuf AnyText in
  parse_end lexbuf;
  text

let parse_scalar_value lexbuf value =
  let identifier = pop_token lexbuf Identifier in
  Scalar { value; identifier }

let parse_binary_vector lexbuf =
  let value = pop_token lexbuf BinaryNumber in
  let identifier = pop_token lexbuf Identifier in
  BinaryVector { value; identifier }

let parse_real_vector _ = RealVector "TODO"

let parse_value_changes lexbuf =
  let[@tail_mod_cons] rec aux () =
    match lex_value_change lexbuf with
    | None ->
        parse_error lexbuf "Value Change"
          "TODO" (* TODO: Add parser error handling *)
    | Some (Value x) -> parse_scalar_value lexbuf x :: aux ()
    | Some BinaryVector -> parse_binary_vector lexbuf :: aux ()
    | Some RealVector -> parse_real_vector lexbuf :: aux ()
    | Some End -> []
  in
  aux ()

let parse_simulation_time lexbuf =
  let time = pop_token lexbuf Number in
  SimulationTime time

let parse_scope lexbuf =
  let scope_type =
    match pop_token lexbuf Identifier with
    | "module" -> Module
    | "task" -> Task
    | "function" -> Function
    | "begin" -> Begin
    | "fork" -> Fork
    | x -> parse_error lexbuf "{module, task, function, begin, fork}" x
  in
  let identifier = pop_token lexbuf Identifier in
  parse_end lexbuf;
  Scope { scope_type; identifier }

let parse_timescale lexbuf =
  let number =
    let token = pop_token lexbuf Number in
    match token with
    | (1 | 10 | 100) as x -> x
    | x -> parse_error lexbuf "{1, 10, 100}" (string_of_int x)
  in
  let time_unit =
    match pop_token lexbuf Identifier with
    | "s" -> Second
    | "ms" -> Millisecond
    | "us" -> Microsecond
    | "ns" -> Nanosecond
    | "ps" -> Picosecond
    | "fs" -> Femtosecond
    | x -> parse_error lexbuf "{s, ms, us, ns, ps, fs}" x
  in
  parse_end lexbuf;
  Timescale { number; time_unit }

let parse_var lexbuf =
  let var_type =
    match pop_token lexbuf Identifier with
    | "event" -> Event
    | "integer" -> Integer
    | "parameter" -> Parameter
    | "real" -> Real
    | "reg" -> Reg
    | "supply0" -> Supply0
    | "supply1" -> Supply1
    | "time" -> Time
    | "tri" -> Tri
    | "triand" -> TriAnd
    | "trior" -> TriOr
    | "trireg" -> TriReg
    | "tri0" -> Tri0
    | "tri1" -> Tri1
    | "wand" -> WAnd
    | "wire" -> Wire
    | "wor" -> WOr
    | x ->
        parse_error lexbuf
          "{event, integer, parameter, real, reg, supply0, supply1, time, tri, \
           triand, trior, trireg, tri0, tri1, wand, wire, wor}"
          x
  in
  let size = pop_token lexbuf Number in
  let identifier = pop_token lexbuf Identifier in
  let reference = pop_token lexbuf Reference in
  parse_end lexbuf;
  Var { var_type; size; identifier; reference }

let next_declaration_cmd lexbuf =
  match lex_declaration lexbuf with
  | None -> parse_error lexbuf "A keyword" "TODO"
  | Some keyword -> (
      match keyword with
      | Date -> Date (parse_any_text lexbuf)
      | Version -> Version (parse_any_text lexbuf)
      | Scope -> parse_scope lexbuf
      | Timescale -> parse_timescale lexbuf
      | Upscope ->
          parse_end lexbuf;
          Upscope
      | Var -> parse_var lexbuf
      | Comment -> Comment (parse_any_text lexbuf)
      | EndDefinitions ->
          parse_end lexbuf;
          EndDefinitions)

let seq_of_declaration lexbuf =
  Seq.unfold
    (fun lexbuf ->
      match next_declaration_cmd lexbuf with
      | exception EndOfFile -> None
      | EndDefinitions -> None
      | x -> Some (x, lexbuf))
    lexbuf

let next_simulation_cmd lexbuf =
  let parse_token (token : simulation_token) : simulation_cmd =
    match token with
    | Comment -> Comment (parse_any_text lexbuf)
    | DumpAll -> DumpAll (parse_value_changes lexbuf)
    | DumpOff -> DumpOff (parse_value_changes lexbuf)
    | DumpOn -> DumpOn (parse_value_changes lexbuf)
    | DumpVars -> DumpVars (parse_value_changes lexbuf)
    | SimulationTime -> parse_simulation_time lexbuf
    | Value x -> ValueChange (parse_scalar_value lexbuf x)
    | BinaryVector -> ValueChange (parse_binary_vector lexbuf)
    | RealVector -> ValueChange (parse_real_vector lexbuf)
  in
  match lex_simulation lexbuf with
  | None -> parse_error lexbuf "Simulation command" "TODO"
  | Some token -> parse_token token

let seq_of_simulation lexbuf =
  Seq.unfold
    (fun lexbuf ->
      match next_simulation_cmd lexbuf with
      | exception EndOfFile -> None
      | x -> Some (x, lexbuf))
    lexbuf

let parse_ast lexbuf =
  let declarations = lexbuf |> seq_of_declaration |> List.of_seq in
  let simulation = lexbuf |> seq_of_simulation |> List.of_seq in
  { declarations; simulation }
