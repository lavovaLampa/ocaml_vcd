type binary_value = Zero | One | X | Z | U [@@deriving show]

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

exception EndOfFile

type _ token_type =
  | End : unit token_type
  | Identifier : string token_type
  | Reference : string token_type
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
let binary_char = [%sedlex.regexp? Chars "01uUxXzZ"]
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

let reference =
  [%sedlex.regexp?
    ( identifier,
      Opt
        ( opt_delimiters,
          '[',
          opt_delimiters,
          decimal_number,
          Opt (':', opt_delimiters, decimal_number),
          opt_delimiters,
          ']' ) )]

let string_of_token lexbuf = Sedlexing.Utf8.lexeme lexbuf

let parse_bin_value ch =
  match ch with
  | 'x' | 'X' -> X
  | 'Z' | 'z' -> Z
  | 'U' | 'u' -> U
  | '1' -> One
  | '0' -> Zero
  | _ -> failwith "Unexpected"

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
          | reference -> Some (string_of_token lexbuf)
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
