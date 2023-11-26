open Lexer

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

type declaration =
  | Comment of string
  | Date of string
  | Version of string
  | Scope of { scope_type : scope_type; identifier : string }
  | Timescale of { number : int; time_unit : time_unit }
  | Upscope
  | Var of {
      var_type : var_type;
      size : int;
      identifier : string;
      reference : string;
    }
[@@deriving show]

let pp_array pp_item fmt items =
  Ppx_show_runtime.pp_list pp_item fmt (Array.to_list items)

type simulation =
  | Comment of string
  | DumpAll of string
  | DumpOff of string
  | DumpOn of string
  | DumpVars of string
  | SimulationTime of int
  | ScalarValue of { value : binary_value; identifier : string }
  | BinaryVectorValue of { value : binary_value array; identifier : string }
  | RealVectorValue of string
[@@deriving show]

type vcd_ast = { declarations : declaration list; simulation : simulation list }
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

let parse_date lexbuf = Date (parse_any_text lexbuf)
let parse_version lexbuf = Version (parse_any_text lexbuf)
let parse_dump_all lexbuf = DumpAll (parse_any_text lexbuf)

(* TODO: Implement properly *)
let parse_dump_on lexbuf = DumpOn (parse_any_text lexbuf)

(* TODO: Implement propertly *)
let parse_dump_off lexbuf = DumpOff (parse_any_text lexbuf)

(* TODO: Implement propertly *)
let parse_dump_vars lexbuf = DumpVars (parse_any_text lexbuf)
(* TODO: Implement propertly *)

let parse_simulation_time lexbuf =
  let time = pop_token lexbuf Number in
  SimulationTime time

let parse_scalar_value lexbuf value =
  let identifier = pop_token lexbuf Identifier in
  ScalarValue { value; identifier }

let parse_binary_vector lexbuf =
  let value = pop_token lexbuf BinaryNumber in
  let identifier = pop_token lexbuf Identifier in
  BinaryVectorValue { value; identifier }

let parse_real_vector _ = RealVectorValue "TODO"

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

let parse_upscope lexbuf =
  parse_end lexbuf;
  Upscope

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

let[@tail_mod_cons] rec parse_declaration lexbuf =
  let parse_keyword (keyword : declaration_token) : declaration =
    match keyword with
    | Comment ->
        let text = pop_token lexbuf AnyText in
        parse_end lexbuf;
        Comment text
    | Date -> parse_date lexbuf
    | Version -> parse_version lexbuf
    | Scope -> parse_scope lexbuf
    | Timescale -> parse_timescale lexbuf
    | Upscope -> parse_upscope lexbuf
    | Var -> parse_var lexbuf
    | EndDefinitions ->
        failwith "Invariant failure!" (* Should be handled beforehand *)
  in
  match lex_declaration lexbuf with
  | exception EndOfFile -> []
  | None -> parse_error lexbuf "A keyword" "TODO"
  | Some token -> (
      match token with
      | EndDefinitions ->
          parse_end lexbuf;
          []
      | x ->
          let result = parse_keyword x in
          result :: parse_declaration lexbuf)

let[@tail_mod_cons] rec parse_simulation lexbuf =
  let parse_token (token : simulation_token) : simulation =
    match token with
    | Comment ->
        let text = pop_token lexbuf AnyText in
        parse_end lexbuf;
        Comment text
    | DumpAll -> parse_dump_all lexbuf
    | DumpOff -> parse_dump_off lexbuf
    | DumpOn -> parse_dump_on lexbuf
    | DumpVars -> parse_dump_vars lexbuf
    | SimulationTime -> parse_simulation_time lexbuf
    | Value x -> parse_scalar_value lexbuf x
    | BinaryVector -> parse_binary_vector lexbuf
    | RealVector -> parse_real_vector lexbuf
  in
  match lex_simulation lexbuf with
  | exception EndOfFile -> []
  | None -> parse_error lexbuf "Simulation command" "TODO"
  | Some token ->
      let result = parse_token token in
      result :: parse_simulation lexbuf

let parse_ast lexbuf =
  let declarations = parse_declaration lexbuf in
  let simulation = parse_simulation lexbuf in
  { declarations; simulation }
