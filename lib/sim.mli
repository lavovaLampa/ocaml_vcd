open Internal

val pp_array :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit

type value_change =
  | Scalar of {
      identifier : string;
      var : Vcd.scoped_var list;
      value : Lexer.binary_value;
    }
  | BinaryVector of {
      identifier : string;
      var : Vcd.scoped_var list;
      value : Lexer.binary_value array;
    }
  | RealVector of {
      identifier : string;
      var : Vcd.scoped_var list;
      value : string;
    }
[@@deriving show]

val seq_of_sim : Vcd.t -> (int * value_change list) Seq.t
(** [seq_of_sim vcd] returns ephemeral, affine sequence of simulation values
    grouped by time
*)
