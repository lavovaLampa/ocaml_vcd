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

val pp_array :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit

type simulation =
  | Comment of string
  | DumpAll of string
  | DumpOff of string
  | DumpOn of string
  | DumpVars of string
  | SimulationTime of int
  | ScalarValue of { value : Lexer.binary_value; identifier : string }
  | BinaryVectorValue of {
      value : Lexer.binary_value array;
      identifier : string;
    }
  | RealVectorValue of string
[@@deriving show]

type vcd_ast = { declarations : declaration list; simulation : simulation list }
[@@deriving show]

val parse_error_printer : exn -> string option
val parse_ast : Sedlexing.lexbuf -> vcd_ast
