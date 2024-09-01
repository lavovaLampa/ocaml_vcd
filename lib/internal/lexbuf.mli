type t
(** Lexing buffer *)

type l

val lexbuf_of_string : string -> t
val lexbuf_of_channel : in_channel -> t

val is_eof : t -> bool
(** [is_eof lexbuf] checks if [lexbuf] reached enf-of-file *)

val peek : t -> char option
(** [peek lexbuf] peeks next byte in buffer without consuming it.
    Returns [None] if eof is reached.
*)

val pop : t -> char option
(** [pop lexbuf] pops/consumes next byte in buffer.
    Returns [None] if eof is reached.
*)

val lexeme : t -> string
(** [lexeme lexbuf] returns current lexeme as a string *)

val consume_lexeme : t -> string option
(** [consume_lexeme lexbuf] returns and consumes current lexeme as a string *)

val trim_lexeme : t -> unit
(** [trim_lexeme lexbuf] consumes current lexeme *)
