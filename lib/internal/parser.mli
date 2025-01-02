open Lexer

exception
  ParseError of { expected : string; got : string; position : Lexing.position }

type scope = { scope_type : Lexer.scope; identifier : string option }
[@@deriving show]

type timescale = { value : int; unit : time_unit } [@@deriving show]
type 'a parse_result = Ok of 'a | EndOfFile | Err of Lexing.position

type var = {
  var_type : var_type;
  size : int;  (** Variable size in bits *)
  identifier : string;  (** Variable identifier in VCD file *)
  reference : reference;  (** Name of the variable in the model *)
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

val next_declaration_cmd : Lexing.lexbuf -> declaration_cmd parse_result
val seq_of_declaration : Lexing.lexbuf -> declaration_cmd Seq.t
val next_simulation_cmd : Lexing.lexbuf -> simulation_cmd parse_result
val seq_of_simulation : Lexing.lexbuf -> simulation_cmd Seq.t
val parse_ast : Lexing.lexbuf -> vcd_ast
