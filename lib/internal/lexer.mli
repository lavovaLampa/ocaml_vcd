type binary_value = Zero | One | X | Z | U | Nothing [@@deriving show]

(** VCD declaration section token*)
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

(** VCD simulation section token *)
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

(** value change token (use in $dump* statements) *)
type value_change_token =
  | Value of binary_value
  | BinaryVector
  | RealVector
  | End
[@@deriving show]

type reference =
  | Scalar of string
  | BitSelect of { identifier : string; bit_index : int }
  | Vector of { identifier : string; msb_index : int; lsb_index : int }
[@@deriving show]

type _ token_type =
  | End : unit token_type
  | Identifier : string token_type
  | Reference : reference token_type
  | Number : int token_type
  | BinaryNumber : binary_value array token_type
  | AnyText : string token_type  (** Lexer token type (used in pull mode) *)

exception EndOfFile

val show_token_type : 't. 't token_type -> string
(** [token_type] pretty-printer *)

val lex_identifier : 't. Sedlexing.lexbuf -> 't token_type -> 't option
(** [lex_identifier lexbuf token_type] returns if given [token_type] can be lexed,
    else returns [None] *)

val lex_declaration : Sedlexing.lexbuf -> declaration_token option
(** [lex_declaration lexbuf] returns tokens during declaration phase *)

val lex_simulation : Sedlexing.lexbuf -> simulation_token option
(** [lex_simulation lexbuf] returns tokens during simulation phase *)

val lex_value_change : Sedlexing.lexbuf -> value_change_token option
(** [lex_value_change lexbuf] returns tokens while lexing inside 
    $dumpon/$dumpoff/$dumpall/$dumpvars statement *)
