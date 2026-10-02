type input = { source : string; offset : int }

let withSource src = { source = src; offset = 0 }

let next p =
  if String.length p.source > p.offset then
    let ch = String.get_utf_8_uchar p.source p.offset in
    Ok
      ( { source = p.source; offset = p.offset + Uchar.utf_decode_length ch },
        Uchar.utf_decode_uchar ch )
  else Error "EOF"

let toString p = Printf.sprintf "parser(%d)" p.offset
