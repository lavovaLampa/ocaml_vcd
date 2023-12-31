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

val pp_array :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit

type value_change =
  | Scalar of { value : Lexer.binary_value; identifier : string }
  | BinaryVector of { value : Lexer.binary_value array; identifier : string }
  | RealVector of string
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

val next_declaration_cmd : Sedlexing.lexbuf -> declaration_cmd
val seq_of_declaration : Sedlexing.lexbuf -> declaration_cmd Seq.t
val next_simulation_cmd : Sedlexing.lexbuf -> simulation_cmd
val seq_of_simulation : Sedlexing.lexbuf -> simulation_cmd Seq.t
val parse_ast : Sedlexing.lexbuf -> vcd_ast
val parse_error_printer : exn -> string option
