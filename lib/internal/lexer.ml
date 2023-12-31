type binary_value = Zero | One | X | Z | U | Nothing

let show_binary_value = function
  | Zero -> "0"
  | One -> "1"
  | X -> "X"
  | Z -> "Z"
  | U -> "U"
  | Nothing -> "-"

let pp_binary_value f v = Format.fprintf f "%s" (show_binary_value v)

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

type value_change_token =
  | Value of binary_value
  | BinaryVector
  | RealVector
  | End
[@@deriving show]

exception EndOfFile

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
  | AnyText : string token_type

let show_token_type : type t. t token_type -> string = function
  | End -> "End"
  | Identifier -> "Identifier"
  | Reference -> "Reference"
  | Number -> "Number"
  | BinaryNumber -> "BinaryNumber"
  | AnyText -> "AnyText"

let identifier = [%sedlex.regexp? Plus '!' .. '~']
let binary_char = [%sedlex.regexp? Chars "01uUxXzZ-"]
let binary_number = [%sedlex.regexp? Plus binary_char]
let whitespace = [%sedlex.regexp? '\t' | ' ']
let newline = [%sedlex.regexp? '\n' | "\r\n"]
let delimiter = [%sedlex.regexp? whitespace | newline]
let opt_delimiters = [%sedlex.regexp? Star delimiter]
let non_whitespace = [%sedlex.regexp? Star (Compl (Chars "\n\t \r"))]

let any_text =
  [%sedlex.regexp?
    Star
      ( Star (Compl '$'),
        Opt
          ( '$',
            ( "end", Compl ('$' | '\n' | '\r' | '\t' | ' ')
            | Compl 'e'
            | 'e', Compl 'n'
            | "en", Compl 'd' ) ) )]

(* FIXME: Implement real number regexp *)
let real_number = [%sedlex.regexp? '0']
let decimal_digit = [%sedlex.regexp? '0' .. '9']
let decimal_number = [%sedlex.regexp? Plus decimal_digit]
let string_of_token lexbuf = Sedlexing.Utf8.lexeme lexbuf

let parse_bin_value ch =
  match ch with
  | 'x' | 'X' -> X
  | 'Z' | 'z' -> Z
  | 'U' | 'u' -> U
  | '1' -> One
  | '0' -> Zero
  | '-' -> Nothing
  | _ -> failwith "Unexpected"

let parse_bit_select s =
  let start_index = String.index s '[' in
  let end_index = String.index s ']' in
  let identifier = String.trim (String.sub s 0 start_index) in
  let bit_index =
    int_of_string @@ String.trim
    @@ String.sub s (start_index + 1) (end_index - start_index - 1)
  in
  BitSelect { identifier; bit_index }

let parse_vector s =
  let start_index = String.index s '[' in
  let colon_index = String.index s ':' in
  let end_index = String.index s ']' in
  let identifier = String.trim (String.sub s 0 start_index) in
  let msb_index =
    int_of_string @@ String.trim
    @@ String.sub s (start_index + 1) (colon_index - start_index - 1)
  in
  let lsb_index =
    int_of_string @@ String.trim
    @@ String.sub s (colon_index + 1) (end_index - colon_index - 1)
  in
  Vector { identifier; msb_index; lsb_index }

let lex_bin_value lexbuf =
  Sedlexing.lexeme_char lexbuf 0 |> Uchar.to_char |> parse_bin_value

let rec lex_identifier : type t. Sedlexing.lexbuf -> t token_type -> t option =
 fun lexbuf token_type ->
  match%sedlex lexbuf with
  | eof -> raise EndOfFile
  | Plus (whitespace | newline) -> lex_identifier lexbuf token_type
  | _ -> (
      match token_type with
      | End -> ( match%sedlex lexbuf with "$end" -> Some () | _ -> None)
      | Identifier -> (
          match%sedlex lexbuf with
          | identifier -> Some (string_of_token lexbuf)
          | _ -> None)
      | Reference -> (
          match%sedlex lexbuf with
          | ( identifier,
              opt_delimiters,
              '[',
              opt_delimiters,
              decimal_number,
              opt_delimiters,
              ':',
              opt_delimiters,
              decimal_number,
              opt_delimiters,
              ']' ) ->
              Some (parse_vector @@ string_of_token lexbuf)
          | ( identifier,
              opt_delimiters,
              '[',
              opt_delimiters,
              decimal_number,
              opt_delimiters,
              ']' ) ->
              Some (parse_bit_select @@ string_of_token lexbuf)
          | identifier -> Some (Scalar (string_of_token lexbuf))
          | _ -> None)
      | Number -> (
          match%sedlex lexbuf with
          | decimal_number -> Some (int_of_string @@ string_of_token lexbuf)
          | _ -> None)
      | BinaryNumber -> (
          match%sedlex lexbuf with
          | binary_number ->
              Some
                (lexbuf |> Sedlexing.lexeme
                |> Array.map (fun x -> parse_bin_value @@ Uchar.to_char x))
          | _ -> None)
      | AnyText -> (
          match%sedlex lexbuf with
          | any_text -> Some (string_of_token lexbuf)
          | _ -> None))

let rec lex_declaration lexbuf =
  match%sedlex lexbuf with
  | "$date" -> Some Date
  | "$comment" -> Some Comment
  | "$version" -> Some Version
  | "$enddefinitions" -> Some EndDefinitions
  | "$scope" -> Some Scope
  | "$timescale" -> Some Timescale
  | "$upscope" -> Some Upscope
  | "$var" -> Some Var
  | Plus (whitespace | newline) -> lex_declaration lexbuf
  | eof -> raise EndOfFile
  | _ -> None

let rec lex_simulation lexbuf =
  match%sedlex lexbuf with
  | "$comment" -> Some Comment
  | "$dumpall" -> Some DumpAll
  | "$dumpoff" -> Some DumpOff
  | "$dumpon" -> Some DumpOn
  | "$dumpvars" -> Some DumpVars
  | '#' -> Some SimulationTime
  | binary_char -> Some (Value (lex_bin_value lexbuf))
  | Chars "bB" -> Some BinaryVector
  | Chars "rR" -> Some RealVector
  | Plus (whitespace | newline) -> lex_simulation lexbuf
  | eof -> raise EndOfFile
  | _ -> None

let rec lex_value_change lexbuf =
  match%sedlex lexbuf with
  | binary_char -> Some (Value (lex_bin_value lexbuf))
  | Chars "bB" -> Some BinaryVector
  | Chars "rR" -> Some RealVector
  | "$end" -> Some End
  | Plus (whitespace | newline) -> lex_value_change lexbuf
  | eof -> raise EndOfFile
  | _ -> None
