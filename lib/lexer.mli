type binary_value = Zero | One | X | Z | U [@@deriving show]

type declaration_token =
  | Comment
  | Date
  | EndDefinitions
  | Scope
  | Timescale
  | Upscope
  | Var
  | Version
[@@deriving show]

type simulation_token =
  | Comment
  | DumpAll
  | DumpOn
  | DumpOff
  | DumpVars
  | SimulationTime
  | Value of binary_value
  | BinaryVector
  | RealVector
[@@deriving show]

type _ token_type =
  | End : unit token_type
  | Identifier : string token_type
  | Reference : string token_type
  | Number : int token_type
  | BinaryNumber : binary_value array token_type
  | AnyText : string token_type

exception EndOfFile

val show_token_type : 't. 't token_type -> string
val lex_identifier : 't. Sedlexing.lexbuf -> 't token_type -> 't option
val lex_declaration : Sedlexing.lexbuf -> declaration_token option
val lex_simulation : Sedlexing.lexbuf -> simulation_token option
