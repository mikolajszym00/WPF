(*
  Piszący: Mikołaj Szymański
  Sprawdzający: Michał Garbacz, Jacek Spaliński
*)


module Leftist =
struct

type 'a queue = Node of 'a queue * ('a * int) * 'a queue | Null

let empty = 
  Null
;;

let join q1 q2 =
  let right_h t =
    match t with
    | Null -> 0
    | Node (_, (_, h), _) -> h+1 in

  let rec merge q1 q2 = 
    match q1, q2 with
    | Null, n | n, Null -> n 
    | Node(l1, (p1, h1), r1), Node(l2, (p2, h2), r2) ->
      if p1 < p2 then
        Node(merge r1 (Node(l2, (p2, h2), r2)), (p1, right_h l1), l1)
      else 
        Node(merge (Node(l1, (p1, h1), r1)) r2, (p2, right_h l2), l2)

  in merge q1 q2
;;

let add (e:'a) (q:'a queue) =
  let node_e = Node(Null, (e, 0), Null) in
  join node_e q
;;  

exception Empty;;

let delete_min (q:'a queue) = 
  match q with
    | Null -> raise Empty
    | Node(l, (p, h), r) ->
      (p, join l r)
;;

let is_empty (q:'a queue) = 
  match q with
  | Null -> true
  | n -> false
;;
  
end
