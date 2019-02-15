module D = Decoders [@@program]

type table_node = int

module Decode (D : D.Decode.S) = struct


  open D
  let intPairs : (int * int) list decoder =
    list (list int >>= (fun x ->
        match x with
        | y1::y2::[] -> succeed (y1, y2)
        | _ -> fail "expected 2 elements for a pair"))

end [@@program]

type person_table =
  { name: string
  ; table: int
  }

module Encode (E : D.Encode.S) = struct
  open E
  let intPairs : (int * int) list encoder = fun xs ->
    xs |> list (fun x -> [fst x; snd x] |> list (fun y  -> int y))

  let person_node_of_pair : person_table encoder = fun x ->
    obj [("id", string x.name)
        ;("group", int x.table)]

  let table_node_of_pair : person_table encoder = fun x ->
    obj [("id", string (Printf.sprintf "Table %d" x.table))
        ;("group", int x.table)]

  let person_link_of_pair : person_table encoder = fun x ->
    obj [("source", string (Printf.sprintf "Table %d" x.table))
        ;("target", string x.name)
        ;("value", int 5)]

  type person_or_table =
    | Person of person_table
    | Table of person_table

  let node_of_person_or_table_pair : person_or_table encoder = function
    | Person p -> person_node_of_pair p
    | Table t -> table_node_of_pair t

  let graph_of_pairs : person_table list encoder = fun xs ->
    let persons =
      xs
      |> CCList.map (fun x -> Person x)
    in
    let tables =
      xs
      |> CCList.uniq ~eq:(=)
      |> CCList.map (fun t -> Table t)
    in
    obj [("nodes", list node_of_person_or_table_pair (persons @ tables))
        ;("links", (list person_link_of_pair xs))]

end [@@program]
