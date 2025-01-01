{
    type signal_value =
      | One
      | Zero
      | X
      | Z
      | U
    [@@deriving show]

    type reference =
      | Identifier of string
      | BitSelect of { identifier : string; bit_select_index : int }
      | Slice of { identifier : string; msb_index : int; lsb_index : int }
    [@@deriving show]

    let identifier_of_reference ref =
      match ref with
      | Identifier s -> s
      | BitSelect { identifier; _ } -> identifier
      | Slice { identifier; _ } -> identifier

    type time_unit =
      | Second
      | Milisecond
      | Microsecond
      | Nanosecond
      | Picosecond
      | Femtosecond
    [@@deriving show]

    type scope = Begin | Fork | Function | Module | Task
    [@@deriving show]

    type var_type =
      | Event
      | Integer
      | Parameter
      | Real
      | Realtime
      | Reg
      | Supply0
      | Supply1
      | Time
      | Tri
      | TriAnd
      | TriOr
      | TriReg
      | Tri0
      | Tri1
      | WAnd
      | Wire
      | WOr
    [@@deriving show]

    type 'a value_change = { identifier : string; value : 'a } [@@deriving show]

    type token =
      | Comment of string
      | Date of string
      | EndDefinitions
      | Scope of { scope_type : scope; identifier : string }
      | Timescale of { value : int; unit : time_unit }
      | Upscope
      | Var of {
          var_type : var_type;
          size : int;
          identifier : string;
          reference : reference
        }
      | Version of string
      | DumpAll
      | DumpOff
      | DumpOn
      | DumpVars
      | SimulationTime of int
      | ScalarValue of char value_change
      | BinaryVector of string value_change
      | RealVector of string value_change
      | EOF
    [@@deriving show]
}

let decimal_digit = ['0' - '9']
let decimal_number = decimal_digit+
let binary_digit = ['0' '1']
let binary_number = binary_digit+
let real_number = ['+' '-']? decimal_digit+ ('.' decimal_digit+)? ('e' ['+' '-'] decimal_digit+)?
let identifier = ['!' - '~']+
let any_string = ([^ '$'] | "$" [^ 'e'] | "$e" [^ 'n'] | "$en" [^ 'd'])*
let w = [' ' '\t' '\n']
let vector_val = (binary_digit | ['u' 'U' 'z' 'Z' 'x' 'X'])

rule declaration =
    parse
    | "$comment" (any_string as t) "$end"                             { Comment t }
    | "$date" (any_string as t) "$end"                                { Date t }
    | "$version" (any_string as t) "$end"                             { Version t }
    | "$enddefinitions" w* "$end"                                     { EndDefinitions }
    | "$scope"                                                        { scope lexbuf }
    | "$timescale"                                                    { timescale lexbuf }
    | "$upscope" w* "$end"                                            { Upscope }
    | "$var"                                                          { var lexbuf }
    | w+                                                              { declaration lexbuf }
    | eof                                                             { EOF }
and simulation =
    parse
    | "$comment" (any_string as t) "$end"                  { Comment t }
    | "$dumpall" w* "$end"                                 { DumpAll }
    | "$dumpoff" w* "$end"                                 { DumpOff }
    | "$dumpon" w* "$end"                                  { DumpOn }
    | "$dumpvars" w* "$end"                                { DumpVars }
    | "#" (decimal_number as n)                            { SimulationTime (int_of_string n) }
    | "0" (identifier as id)                               { ScalarValue {identifier=id; value='0'} }
    | "1" (identifier as id)                               { ScalarValue {identifier=id; value='1'} }
    | ['x' 'X'] (identifier as id)                         { ScalarValue {identifier=id; value='X'} }
    | ['z' 'Z'] (identifier as id)                         { ScalarValue {identifier=id; value='Z'} }
    | ['u' 'U'] (identifier as id)                         { ScalarValue {identifier=id; value='U'} }
    | ['b' 'B'] (vector_val+ as b) w+ (identifier as id)   { BinaryVector {identifier=id; value=b} }
    | ['r' 'R'] (real_number as r) w+ (identifier as id)   { RealVector {identifier=id; value=r} }
    | w+                                                   { simulation lexbuf }
    | eof                                                  { EOF }
and scope =
    parse 
    | w* "begin" w* (identifier as id) w* "$end"    { Scope { scope_type=Begin; identifier=id } }
    | w* "fork" w* (identifier as id) w* "$end"     { Scope { scope_type=Fork; identifier=id } }
    | w* "function" w* (identifier as id) w* "$end" { Scope { scope_type=Function; identifier=id } } 
    | w* "module" w* (identifier as id) w* "$end"   { Scope { scope_type=Module; identifier=id } }
    | w* "task" w* (identifier as id) w* "$end"     { Scope { scope_type=Task; identifier=id } }
and timescale =
    parse
    | w* (("1" | "10" | "100") as t) w* "s" w* "$end"  { Timescale { value=int_of_string t; unit=Second } }
    | w* (("1" | "10" | "100") as t) w* "ms" w* "$end" { Timescale { value=int_of_string t; unit=Milisecond } }
    | w* (("1" | "10" | "100") as t) w* "us" w* "$end" { Timescale { value=int_of_string t; unit=Microsecond } }
    | w* (("1" | "10" | "100") as t) w* "ns" w* "$end" { Timescale { value=int_of_string t; unit=Nanosecond } }
    | w* (("1" | "10" | "100") as t) w* "ps" w* "$end" { Timescale { value=int_of_string t; unit=Picosecond } }
    | w* (("1" | "10" | "100") as t) w* "fs" w* "$end" { Timescale { value=int_of_string t; unit=Femtosecond } }
and var =
    parse
    | w* "event" w* (decimal_number as size) w* (identifier as id)     { var_reference Event (int_of_string size) id lexbuf }
    | w* "integer" w* (decimal_number as size) w* (identifier as id)   { var_reference Integer (int_of_string size) id lexbuf }
    | w* "parameter" w* (decimal_number as size) w* (identifier as id) { var_reference Parameter (int_of_string size) id lexbuf }
    | w* "real" w* (decimal_number as size) w* (identifier as id)      { var_reference Real (int_of_string size) id lexbuf }
    | w* "realtime" w* (decimal_number as size) w* (identifier as id)  { var_reference Realtime (int_of_string size) id lexbuf }
    | w* "reg" w* (decimal_number as size) w* (identifier as id)       { var_reference Reg (int_of_string size) id lexbuf }
    | w* "supply0" w* (decimal_number as size) w* (identifier as id)   { var_reference Supply0 (int_of_string size) id lexbuf }
    | w* "supply1" w* (decimal_number as size) w* (identifier as id)   { var_reference Supply1 (int_of_string size) id lexbuf }
    | w* "time" w* (decimal_number as size) w* (identifier as id)      { var_reference Time (int_of_string size) id lexbuf }
    | w* "tri" w* (decimal_number as size) w* (identifier as id)       { var_reference Tri (int_of_string size) id lexbuf }
    | w* "triand" w* (decimal_number as size) w* (identifier as id)    { var_reference TriAnd (int_of_string size) id lexbuf }
    | w* "trior" w* (decimal_number as size) w* (identifier as id)     { var_reference TriOr (int_of_string size) id lexbuf }
    | w* "trireg" w* (decimal_number as size) w* (identifier as id)    { var_reference TriReg (int_of_string size) id lexbuf }
    | w* "tri0" w* (decimal_number as size) w* (identifier as id)      { var_reference Tri0 (int_of_string size) id lexbuf }
    | w* "tri1" w* (decimal_number as size) w* (identifier as id)      { var_reference Tri1 (int_of_string size) id lexbuf }
    | w* "wand" w* (decimal_number as size) w* (identifier as id)      { var_reference WAnd (int_of_string size) id lexbuf }
    | w* "wire" w* (decimal_number as size) w* (identifier as id)      { var_reference Wire (int_of_string size) id lexbuf }
    | w* "wor" w* (decimal_number as size) w* (identifier as id)       { var_reference WOr (int_of_string size) id lexbuf }
and var_reference var_type size id_code =
    parse
    | w* (identifier as identifier) w* "[" w* (decimal_number as msb) w* ":" w* (decimal_number as lsb) w* "]" w* "$end" {
            Var {
                var_type;
                size;
                identifier=id_code;
                reference=Slice {
                    identifier;
                    msb_index=int_of_string msb;
                    lsb_index=int_of_string lsb
                }
            }            
        }
    | w* (identifier as identifier) w* "[" w* (decimal_number as bs) w* "]" w* "$end" {
            Var {
                var_type;
                size;
                identifier=id_code;
                reference= BitSelect {
                    identifier;
                    bit_select_index=int_of_string bs
                }            
            }
        }
    | w* (identifier as identifier) w* "$end" {
            Var {
                var_type;
                size;
                identifier=id_code;
                reference=Identifier identifier
            }
        }
