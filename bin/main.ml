let parser = Parser.withSource "こんにちはhello"

let rec printParser = function
    | (Ok (p, ch)) ->
        let b = Buffer.create 4 in
        Buffer.add_utf_8_uchar b ch;
        Printf.printf "%s: %s\n" (Parser.toString p) (Buffer.contents b);
        p |> Parser.next |> printParser
    | (Error err) -> Printf.printf "err: %s\n" err

let () = parser |> Parser.next |> printParser
