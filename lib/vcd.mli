open Internal

type src_type = String of string | File of string  (** Input source type *)

type scope = { scope : Parser.scope; parent : scope option } [@@deriving show]
(** Scope hierarchy *)

type scoped_var = { var : Parser.var; scope : scope option } [@@deriving show]
(** Scoped variable *)

type src = {
  src : src_type;
  sim_byte_offset : int;  (** Byte offset into simulation commands *)
}
(** Internal parsing state *)

type declarations = {
  declarations : Parser.declaration_cmd list;
  timescale : Parser.timescale;
  date : string option;
  version : string option;
  id_to_var : (string, scoped_var) Hashtbl.t;
}
(** Parsed declarations *)

type t = { src : src; declarations : declarations }
(** VCD parser type *)

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
(** [comments vcd] returns list of declaration comments.

    For comments in simulation part, use module [Sim].
*)

val string_of_var : ?sep:string -> scoped_var -> string
(** [string_of_var ?(sep = ".") vcd var] creates a string representation
  of [var], concatenating parent scopes with separator [sep]. *)

val var_of_identifier : t -> string -> scoped_var list
(** [var_of_identifier vcd identifier] returns all variable bound to [identifier]. *)

val from_utf8_file : string -> t
(** [from_utf8_file file] creates a fresh parser from UTF-8 encoded [file] *)

val from_utf8_string : string -> t
(** [from_utf8_string string] creates a fresh parser from UTF-8 encoded [string] *)
