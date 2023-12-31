open Internal
open Util

type src_type = String of string | File of string  (** Input source type *)
type scope = { scope : Parser.scope; parent : scope option } [@@deriving show]
type scoped_var = { var : Parser.var; scope : scope option } [@@deriving show]

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

type t = { src : src; declarations : declarations }

let build_var_hashtbl ?(size = 100) declarations =
  let open Parser in
  let hashtbl = Hashtbl.create size in
  let aux parent cmd =
    match cmd with
    | Scope scope -> Some { scope; parent }
    | Upscope -> (
        match parent with
        | None -> failwith "Unexpected"
        | Some { parent; _ } -> parent)
    | Var var ->
        Hashtbl.add hashtbl var.identifier { var; scope = parent };
        parent
    | _ -> parent
  in
  let _ = List.fold_left aux None declarations in
  hashtbl

let get_timescale =
  ensure_one "timescale"
  % List.filter_map (function Parser.Timescale t -> Some t | _ -> None)

let get_version =
  ensure_one_or_none "version"
  % List.filter_map (function Parser.Version v -> Some v | _ -> None)

let get_date =
  ensure_one_or_none "date"
  % List.filter_map (function Parser.Date d -> Some d | _ -> None)

let make src =
  let init_state lexbuf =
    let declarations = lexbuf |> Parser.seq_of_declaration |> List.of_seq in
    let id_to_var = build_var_hashtbl declarations in
    let sim_byte_offset = Sedlexing.lexeme_bytes_end lexbuf in
    let timescale = get_timescale declarations in
    let date = get_date declarations in
    let version = get_version declarations in
    {
      src = { src; sim_byte_offset };
      declarations = { declarations; timescale; date; version; id_to_var };
    }
  in
  match src with
  | String s -> init_state (Sedlexing.Utf8.from_string s)
  | File f ->
      In_channel.with_open_bin f (init_state % Sedlexing.Utf8.from_channel)

let declarations { declarations = { declarations; _ }; _ } = declarations
let version { declarations = { version; _ }; _ } = version
let date { declarations = { date; _ }; _ } = date
let timescale { declarations = { timescale; _ }; _ } = timescale

let variables { declarations = { id_to_var; _ }; _ } =
  id_to_var |> Hashtbl.to_seq_values |> List.of_seq

let identifiers { declarations = { id_to_var; _ }; _ } =
  id_to_var |> Hashtbl.to_seq_keys |> List.of_seq

let comments { declarations = { declarations; _ }; _ } =
  declarations
  |> List.filter_map (function
       | (Comment x : Parser.declaration_cmd) -> Some x
       | _ -> None)

let string_of_var ?(sep = ".") { var; scope } =
  let rec string_of_scope scope =
    match scope with
    | None -> []
    | Some { scope; parent } -> scope.identifier :: string_of_scope parent
  in
  String.concat sep @@ List.rev
  @@ (Lexer.show_reference var.reference :: string_of_scope scope)

let var_of_identifier { declarations = { id_to_var; _ }; _ } id =
  Hashtbl.find_all id_to_var id

let from_utf8_file file = make (File file)
let from_utf8_string string = make (String string)
