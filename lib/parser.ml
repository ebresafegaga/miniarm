let print_error_position lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "Line:%d Position:%d" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let from_lexbuf lexbuf =
  match Grammar.program Lexer.read_token lexbuf with
  | syntax -> Ok syntax
  | exception Grammar.Error ->
      let msg =
        Printf.sprintf "Parser Error at %s" (print_error_position lexbuf)
      in
      Error msg
  | exception Lexer.SyntaxError (_p, e) ->
      let msg =
        Printf.sprintf "Syntax Error: %s at %s" e (print_error_position lexbuf)
      in
      Error msg

let from_string s = s |> Lexing.from_string |> from_lexbuf
let from_file name = name |> open_in |> Lexing.from_channel |> from_lexbuf

module To_test = struct
  let parse = from_string
end
