open App_types

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

let nodes_of_assignments (xs : assignment list) : node list =
  (* person nodes and table nodes *)
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
