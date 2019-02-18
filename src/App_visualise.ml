open App_types

external updateNodes: string -> Js.Json.t -> unit = "updateNodes" [@@bs.module "./forceGraph"]
external updateLinks: string -> Js.Json.t -> unit = "updateLinks" [@@bs.module "./forceGraph"]

type node_ref =
  | Table of int
  | Guest of guest

type node =
  { ref_: node_ref
  ; group: int
  }

type link =
  { source: node_ref
  ; target: node_ref
  }

module Encode = struct
  open Decoders_bs.Encode

  let node_ref = function
    | Table t -> obj [("type", string "table"); ("id", int t)]
    | Guest g -> obj [("type", string "guest"); ("id", int g.id); ("name", string g.name)]

  let node (x : node) =
    obj [("ref", node_ref x.ref_)
        ;("group", int x.group)
        ]

  let link (x : link) =
    obj [("source", node_ref x.source)
        ;("target", node_ref x.target)
        ]
end

let current_nodes : node list ref = ref ([])

let nodes_of_assignments (xs : assignment list) : node list =
  (* person nodes and table nodes *)
  (* sorted for stability of comparison *)
  ((xs
    |> List.map (fun a ->
        { ref_= Guest a.guest
        ; group = a.table
        }
      )) @
   (xs
    |> List.sort_uniq (fun (a: assignment) (b: assignment) -> compare a.table b.table)
    |> List.map (fun (a : assignment) ->
        { ref_ = Table a.table
        ; group = a.table
        }
      )))
  |> List.sort compare

let links_of_assignments (xs : assignment list) : link list =
  (* person to table links, and person to person links to keep them apart on a single table *)
  (xs
   |> List.map (fun (a : assignment) ->
       { source = Guest a.guest
       ; target = Table a.table
       })) @
  (xs
   |> List.map (fun (a : assignment) ->
       xs
       |> List.filter (fun (b : assignment) -> a.table = b.table)
       |> List.map (fun (b : assignment) ->
           (a.guest , b.guest)
         )
     )
   |> List.concat
   |> List.sort_uniq (fun ((a1, a2) : guest * guest) ((b1, b2) : guest * guest) ->
       let mins = compare (min a1.id a2.id) (min b1.id b2.id) in
       if mins <> 0 then
         mins
       else
         compare (max a1.id a2.id) (max b1.id b2.id)
     )
   |> List.map (fun (a, b : guest * guest) ->
       { source = Guest a
       ; target = Guest b
       }
     ))

let handle_new_assignments (selector : string) (xs : assignment list) =
  let new_nodes = nodes_of_assignments xs in
  let new_links = links_of_assignments xs in
  (if new_nodes <> !current_nodes then
     updateNodes selector (Decoders_bs.Encode.encode_value (Decoders_bs.Encode.list Encode.node) new_nodes));

  updateLinks selector (Decoders_bs.Encode.encode_value (Decoders_bs.Encode.list Encode.link) new_links)
