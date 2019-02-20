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

let nodes_of_assignments (xs : assignment list) : node list =
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
      ))
  )

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

external document : Dom.document = "" [@@bs.val]

module NodeSvg = struct
  type svg
  type t

  external query_svg : Dom.document -> string -> svg = "querySelector" [@@bs.send]
  external query_all_svg : svg -> string -> t array = "querySelectorAll" [@@bs.send]
  external create_element_ns : Dom.document -> string -> string -> t = "createElementNS" [@@bs.send]
  external append_node : svg -> t -> unit = "appendChild" [@@bs.send]
  external remove_node : svg -> t -> unit = "removeChild" [@@bs.send]
  external set_string_attribute : t -> string -> string -> unit = "setAttribute" [@@bs.send]

  let create_svg_el (el_name : string) =
    create_element_ns document "http://www.w3.org/2000/svg" el_name

  let append_g (svg : svg) : t =
    let g = create_svg_el "g" in
    append_node svg g;
    g

  let delete_g (svg : svg) (g : t) =
    remove_node svg g

  let set_x (t : t) (x : float) =
    set_string_attribute t "x" (string_of_int (int_of_float x))

  let all_gs (svg : svg) : t array =
    query_all_svg svg "g"

end


type 'a with_sim =
  { t: 'a
  ; x : float
  ; y : float
  ; vx : float
  ; vy : float
  ; nodeRef: NodeSvg.t option ref
  }

module NodeRefCompare =
  Belt.Id.MakeComparable(struct
    type t = node_ref
    let cmp = compare
  end)


type sim_nodes = (node_ref, node with_sim, NodeRefCompare.identity) Belt.Map.t

let empty_sim_nodes : sim_nodes = Belt.Map.make ~id:(module NodeRefCompare)

let with_default_sim (a : node) : node with_sim =
  { t = a
  ; x = 0.
  ; y = 0.
  ; vx = 0.
  ; vy = 0.
  ; nodeRef = ref(None)
  }

let merge_with_sim_nodes (s : sim_nodes) (assignments : assignment list) =
  let nodes = nodes_of_assignments assignments in
  let nodes_by_key = Belt.Map.fromArray ~id:(module NodeRefCompare) (
      nodes
      |> List.map (fun n -> (n.ref_, with_default_sim n))
      |> Array.of_list
    ) in
  Belt.Map.merge s nodes_by_key (fun _k a b -> match (a, b) with
      | None, None -> None
      | (Some _), None -> None
      | None, (Some a) -> Some a
      | (Some a), (Some _b) -> Some a
    )

let append_new (svg : NodeSvg.svg) (s : sim_nodes) =
  Belt.Map.forEach s (fun _k v ->
      match !(v.nodeRef) with
      | None ->
        begin
          let r = NodeSvg.append_g svg in
          v.nodeRef := Some r;
          ()
        end

      | Some _ -> ()

    )

let remove_old (svg : NodeSvg.svg) (s : sim_nodes) =
  let all_gs = NodeSvg.all_gs svg |> Array.to_list in
  let unreffed = all_gs |> List.filter (fun g ->
      Belt.Map.every s (fun _k v ->
          match !(v.nodeRef) with
          | None -> true
          | Some r ->
            r <> g
        )
    )
  in
  Belt.List.forEach unreffed (fun g ->
      NodeSvg.remove_node svg g
    )


let animate (selector : string) (s : sim_nodes) =
  let svg = NodeSvg.query_svg document selector in
  remove_old svg s;
  append_new svg s;

(* let handle_new_assignments (selector : string) (xs : assignment list) =
 *   let new_nodes = nodes_of_assignments xs in
 *   let new_links = links_of_assignments xs in
 *   updateNodes selector (Decoders_bs.Encode.encode_value (Decoders_bs.Encode.list Encode.node) new_nodes);
 *   updateLinks selector (Decoders_bs.Encode.encode_value (Decoders_bs.Encode.list Encode.link) new_links) *)

(* let update (xs : assignment list) =
 *   let new_nodes = nodes_of_assignments xs in
 *   merge_with_sim_nodes new_nodes () *)
