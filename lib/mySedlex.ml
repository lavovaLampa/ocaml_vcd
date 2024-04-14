module type Internal = sig
  type lexbuf

  val __private__next_int : lexbuf -> int
  val backtrack : lexbuf -> int
  val mark : lexbuf -> int -> unit
  val start : lexbuf -> unit
end

module type BasicLexer = sig
  type lexbuf

  val lexeme : lexbuf -> Uchar.t array
  val lexeme_char : lexbuf -> int -> Uchar.t
  val sub_lexeme : lexbuf -> int -> int -> Uchar.t array
  val lexeme_length : lexbuf -> int
  val string_of_lexbuf : lexbuf -> string
  val lexeme_bytes_end : lexbuf -> int
end

module MyUtf8 : sig
  include Internal
  include BasicLexer with type lexbuf := lexbuf
end = struct
  (* TODO: Maybe migrate to circular buffer in the future? *)
  type file_buf = {
    file : In_channel.t;
    mutable buf : bytes;
    mutable offset : int;
    mutable size : int;
    mutable eof : bool;
  }

  (* TODO: Definitely migrate to circular buffer in the future *)
  type uchar_buf = {
    mutable buf : Uchar.t array;
    mutable base : int;
    mutable offset : int;
    mutable size : int;
    mutable eof : bool;
    mutable byte_offset : int;
  }

  type slot = {
    idx : int;
    offset : int;
        (** Computed as [offset - base] as the buffer can be re-aligned and resized *)
  }

  type lexbuf = { file : file_buf; uchar : uchar_buf; mutable slot : slot }

  (* TODO: Check if this makes sense *)
  let uchar_buf_size = 4096 (* 4kB *)
  let file_buf_size = 16_386 (* 16 kB *)
  let _ = assert (file_buf_size >= uchar_buf_size * 4)
  let temp_buf = Buffer.create file_buf_size

  let string_of_lexbuf { uchar = { buf; base; offset; _ }; _ } =
    Buffer.clear temp_buf;
    for i = base to offset - 1 do
      Buffer.add_utf_8_uchar temp_buf buf.(i)
    done;
    Buffer.contents temp_buf

  let backtrack { slot = { idx; offset }; uchar; _ } =
    uchar.offset <- uchar.base + offset;
    idx

  let count_utf8_bytes buf offset len =
    let count = ref 0 in
    for i = offset to offset + len - 1 do
      count := !count + Uchar.utf_8_byte_length buf.(i)
    done;
    !count

  let lexeme_bytes_end { uchar = { buf; base; offset; byte_offset; _ }; _ } =
    byte_offset + count_utf8_bytes buf base (offset - base)

  let mark lexbuf idx =
    let offset = lexbuf.uchar.offset - lexbuf.uchar.base in
    lexbuf.slot <- { offset; idx }

  let start lexbuf =
    lexbuf.uchar.base <- lexbuf.uchar.offset;
    lexbuf.slot <- { idx = -1; offset = lexbuf.uchar.offset }

  let lexeme { uchar = { buf; base; offset; _ }; _ } =
    Array.sub buf base (offset - base)

  let lexeme_char { uchar = { buf; base; offset; _ }; _ } pos =
    if pos >= 0 && pos < offset - base then buf.(base + pos)
    else failwith "Invalid character index"

  let sub_lexeme { uchar = { buf; base; _ }; _ } pos len =
    Array.sub buf (base + pos) len

  let lexeme_length { uchar = { base; offset; _ }; _ } = offset - base
  let utf8_accept_state = 0
  let utf8_reject_state = 1

  let utf8d = [|
    (* The first part of the table maps bytes to character classes that *)
    (* to reduce the size of the transition table and create bitmasks. *)
     0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
     0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
     0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
     0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
     1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;
     7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;  7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;
     8;8;2;2;2;2;2;2;2;2;2;2;2;2;2;2;  2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;
    10;3;3;3;3;3;3;3;3;3;3;3;3;4;3;3; 11;6;6;6;5;8;8;8;8;8;8;8;8;8;8;8;

    (* The second part is a transition table that maps a combination *)
    (* of a state of the automaton and a character class to a state. *)
     0;12;24;36;60;96;84;12;12;12;48;72; 12;12;12;12;12;12;12;12;12;12;12;12;
    12; 0;12;12;12;12;12; 0;12; 0;12;12; 12;24;12;12;12;12;12;24;12;24;12;12;
    12;12;12;12;12;12;12;24;12;12;12;12; 12;24;12;12;12;12;12;12;12;24;12;12;
    12;12;12;12;12;12;12;36;12;36;12;12; 12;36;12;12;12;12;12;36;12;36;12;12;
    12;36;12;12;12;12;12;12;12;12;12;12; 
  |] [@@ocamlformat "disable"]

  let create fjel =
    let file : file_buf =
      {
        file = fjel;
        buf = Bytes.create file_buf_size;
        offset = 0;
        size = 0;
        eof = false;
      }
    in
    let uchar : uchar_buf =
      {
        buf = Array.make uchar_buf_size (Uchar.of_int 0);
        base = 0;
        offset = 0;
        size = 0;
        eof = false;
        byte_offset = 0;
      }
    in
    let slot = { idx = -1; offset = 0 } in
    { file; uchar; slot }

  let decode state codepoint byte =
    let t = utf8d.(byte) in
    codepoint :=
      if !state <> utf8_accept_state then
        byte land 0x3F lor Int.shift_left !codepoint 6
      else Int.shift_right_logical 0xFF t land byte;
    state := utf8d.(256 + !state + t);
    !state
  [@@inline]

  let enlarge_buffers (uchar_buf : uchar_buf) (file_buf : file_buf) =
    let enlarge_uchar_buf () =
      let len = Array.length uchar_buf.buf in
      let new_len = len * 2 in
      let new_buf = Array.make new_len (Uchar.of_int 0) in
      Array.blit uchar_buf.buf 0 new_buf 0 len;
      uchar_buf.buf <- new_buf
    in
    let enlarge_file_buf () =
      let len = Bytes.length file_buf.buf in
      let new_len = len * 2 in
      let new_buf = Bytes.create new_len in
      Bytes.blit file_buf.buf 0 new_buf 0 len;
      file_buf.buf <- new_buf
    in
    enlarge_uchar_buf ();
    enlarge_file_buf ()

  let file_buf_sanity_check ({ buf; size; offset; _ } : file_buf) =
    assert (size <= Bytes.length buf);
    assert (offset <= size);
    assert (0 <= offset)

  let uchar_buf_sanity_check { buf; base; offset; size; byte_offset; _ } =
    assert (size <= Array.length buf);
    assert (offset <= size);
    assert (base <= offset);
    assert (0 <= base);
    assert (0 <= byte_offset)

  let align_file_buffer ({ buf; size; offset; _ } as f : file_buf) =
    file_buf_sanity_check f;
    if offset > 0 then (
      let fill = size - offset in
      Bytes.blit buf offset buf 0 fill;
      f.offset <- 0;
      f.size <- fill);
    file_buf_sanity_check f

  let refill_file_buffer ({ file; buf; size; offset; eof } as f : file_buf) =
    file_buf_sanity_check f;
    if (not eof) && offset >= size then (
      align_file_buffer f;
      let max_read_amount = Bytes.length buf - size in
      let actual_amount = In_channel.input file buf size max_read_amount in
      assert (size + actual_amount <= Bytes.length buf);
      f.eof <- actual_amount < max_read_amount;
      f.size <- size + actual_amount);
    file_buf_sanity_check f

  let align_uchar_buffer ({ buf; base; size; offset; byte_offset; _ } as b) =
    uchar_buf_sanity_check b;
    if base > 0 then (
      let fill = size - base in
      if fill = Array.length buf then failwith "Buffer is not large enough!";
      Array.blit buf base buf 0 fill;
      b.offset <- offset - base;
      b.base <- 0;
      b.size <- fill;
      b.byte_offset <- byte_offset + count_utf8_bytes buf 0 base;
      uchar_buf_sanity_check b)

  let refill_uchar_buffer (file_buf : file_buf) (uchar_buf : uchar_buf) =
    let uchar_buf_len = Array.length uchar_buf.buf in
    let do_refill () =
      file_buf_sanity_check file_buf;
      uchar_buf_sanity_check uchar_buf;

      let last_valid_f_off = ref file_buf.offset in
      let f_off = ref file_buf.offset in
      let u_off = ref uchar_buf.size in
      let codepoint = ref 0 in
      let state = ref 0 in

      while !f_off < file_buf.size && !u_off < uchar_buf_len do
        let result =
          (decode [@inlined]) state codepoint
            (Char.code @@ Bytes.get file_buf.buf !f_off)
        in
        if result = utf8_accept_state then (
          uchar_buf.buf.(!u_off) <- Uchar.unsafe_of_int !codepoint;
          u_off := !u_off + 1;
          last_valid_f_off := !f_off + 1);
        f_off := !f_off + 1
      done;

      (* In case the last byte is not on the UTF-8 boundary *)
      file_buf.offset <- !last_valid_f_off;
      uchar_buf.size <- !u_off;

      file_buf_sanity_check file_buf;
      uchar_buf_sanity_check uchar_buf
    in
    align_uchar_buffer uchar_buf;
    while
      uchar_buf.size < uchar_buf_len
      && (file_buf.offset < file_buf.size || not file_buf.eof)
    do
      refill_file_buffer file_buf;
      do_refill ()
    done;
    uchar_buf_sanity_check uchar_buf;
    file_buf_sanity_check file_buf;
    uchar_buf.eof <- file_buf.offset >= file_buf.size && file_buf.eof

  let rec __private__next_int ({ file; uchar; _ } as t) =
    if uchar.offset < uchar.size then (
      let result = uchar.buf.(uchar.offset) in
      uchar.offset <- uchar.offset + 1;
      Uchar.to_int result)
    else if uchar.eof then -1
    else (
      refill_uchar_buffer file uchar;
      __private__next_int t)
end
