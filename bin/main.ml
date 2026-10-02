let input = Parser.withSource "こんにちはhello"

let rec printInput p =
  match p with
  | Ok (p, ch) ->
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b ch;
      Printf.printf "%s: %s\n" (Parser.toString p) (Buffer.contents b);
      printInput @@ Parser.next p
  | Error err -> Printf.printf "err: %s\n" err

let () = printInput @@ Parser.next input
