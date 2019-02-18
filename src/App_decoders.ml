module D = Decoders [@@program]

open App_types

module Decode (D : D.Decode.S) = struct

  open D
  let intPairs : (int * int) list decoder =
    list (list int >>= (fun x ->
        match x with
        | y1::y2::[] -> succeed (y1, y2)
        | _ -> fail "expected 2 elements for a pair"))

end [@@program]

module Encode (E : D.Encode.S) = struct
  open E
  let intPairs : (int * int) list encoder = fun xs ->
    xs |> list (fun x -> [fst x; snd x] |> list (fun y  -> int y))

  let assignment : assignment encoder = fun x ->
    obj [("guest", string x.guest.name)
        ;("table", int x.table)]

  let assignments : assignment list encoder = fun xs ->
    list assignment xs

end [@@program]
