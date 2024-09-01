type t = {
  mutable size : int;
  mutable buf : bytes;
  mutable offset : int;
  mutable end_offset : int;
  mutable lexeme_offset : int;
  mutable line : int;
  mutable eof : bool;
  refill_fn : bytes -> int -> int -> int;
}

let default_size = 4096

let create refill_fn =
  {
    size = default_size;
    offset = 0;
    end_offset = 0;
    buf = Bytes.create default_size;
    line = 0;
    eof = false;
    lexeme_offset = 0;
    refill_fn;
  }

let zero_offset_align ({ buf; lexeme_offset; offset; end_offset; _ } as lexbuf)
    =
  if lexeme_offset > 0 && lexeme_offset < end_offset then (
    Bytes.blit buf lexeme_offset buf 0 (end_offset - lexeme_offset);
    lexbuf.lexeme_offset <- 0;
    lexbuf.offset <- offset - lexeme_offset;
    lexbuf.end_offset <- end_offset - lexeme_offset)

let resize ({ buf; size; _ } as lexbuf) =
  lexbuf.buf <- Bytes.extend buf 0 size;
  lexbuf.size <- 2 * size

let rec refill ({ buf; size; end_offset; eof; refill_fn; _ } as lexbuf) =
  if not eof then (
    zero_offset_align lexbuf;
    match refill_fn buf end_offset (size - end_offset) with
    | 0 -> lexbuf.eof <- true
    | count when end_offset + count < size ->
        lexbuf.end_offset <- end_offset + count;
        refill lexbuf
    | count -> lexbuf.end_offset <- end_offset + count)

(* TODO: Implement *)
let lexbuf_of_string (s : string) = create (fun buf offset len -> 0)
let lexbuf_of_channel ch = create (In_channel.input ch)

let rec peek ({ buf; offset; end_offset; eof; _ } as lexbuf) =
  if offset < end_offset then Some (Bytes.get buf offset)
  else if eof then None
  else (
    refill lexbuf;
    peek lexbuf)

let consume ({ offset; end_offset; _ } as lexbuf) =
  if offset < end_offset then lexbuf.offset <- offset + 1

let consume_lexeme lexbuf = lexbuf.lexeme_offset <- lexbuf.offset

let lexeme { buf; offset; lexeme_offset; _ } =
  Bytes.sub_string buf offset (offset - lexeme_offset)
