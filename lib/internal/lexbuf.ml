type lexbuf = {
  mutable buf : Bytes.t;
  mutable size : int;
  mutable offset_start : int;
  mutable offset_end : int;
  mutable line : int;
}

let default_size = 4096

let create () =
  {
    size = default_size;
    offset_start = 0;
    offset_end = 0;
    buf = Bytes.create default_size;
    line = 0;
  }

let lexbuf_of_string s = _
let lexbuf_of_channel ch = _
let next lexbuf = _
let lexeme lexbuf = _
