open Internal

type scope = { scope : Parser.scope; parent : scope option } [@@deriving show]
(** Scope hierarchy *)

type scoped_var = { var : Parser.var; scope : scope option } [@@deriving show]
(** Scoped variable *)

type t
(** Internal parsing state *)

val declarations : t -> Parser.declaration_cmd list
(** [declarations vcd] returns list of parsed declarations *)

val version : t -> string option
(** [version vcd] returns version string (if available) *)

val date : t -> string option
(** [date vcd] returns date string (if available) *)

val timescale : t -> Parser.timescale
(** [timescale vcd] returns timescale *)

val variables : t -> scoped_var list
(** [variables vcd] returns defined variables *)

val identifiers : t -> string list
(** [identifiers vcd] returns defined identifiers *)

val comments : t -> string list
(** [comments vcd] returns list of declaration comments. *)

val string_of_var : ?sep:string -> scoped_var -> string
(** [string_of_var ?(sep = ".") vcd var] creates a string representation of
    [var], concatenating parent scopes with separator [sep]. *)

val var_of_identifier : t -> string -> scoped_var list
(** [var_of_identifier vcd identifier] returns all variables bound to
    [identifier]. *)

val seq_of_simulation : t -> Parser.simulation_cmd Seq.t
(** [seq_of_simulation vcd] returns ephemeral, affine sequence of simulation
    values grouped by time *)

val from_file : string -> t
(** [from_file file] creates a fresh parser from UTF-8 encoded [file] *)

val from_channel : In_channel.t -> t
(** [from_channel channel] creates a fresh parser from UTF-8 encoded [channel]
*)

val from_string : string -> t
(** [from_string string] creates a fresh parser from [string] *)

val close : t -> unit
(** [close vcd] closes input channel used for parsing (NOP for string input) *)
