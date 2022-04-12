(*
  Piszący: Mikołaj Szymański
  Sprawdzający: Mikołaj Dziok
*)

exception Cykliczne
;;

type 'a state = Done | Curr | To_visit of 'a list
;;

(* tworzy słownik wszystkich wierzchołków na liście *)
let lst_to_map lst = 
  List.fold_left (fun tree (a, a_list) -> add a (To_visit a_list) tree) empty lst
;;

let rec go_to_nbrs parent lst sorted dict =
  match lst with
  | Curr -> raise Cykliczne
  | Done -> (sorted, dict)
  | To_visit [] -> (parent::sorted, add parent Done dict) 
  (* wszystkie elementy, które mają zostać wykonane po parent zostały już dodane do sorted*)
  | To_visit (a::t) ->
    let (sorted, dict) = 
      if mem a dict then go_to_nbrs a (find a dict) sorted (add a Curr dict) 
      else (a::sorted, dict) in
    go_to_nbrs parent (To_visit t) sorted (add a Done dict)
;;

let topol lst = 
  let dict = lst_to_map lst in
  let loop (s, g) (a, _) = go_to_nbrs a (find a g) s g in

  fst (List.fold_left loop ([], dict) lst)
;;
