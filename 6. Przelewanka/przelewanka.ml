(*
  Piszący: Mikołaj Szymański
  Sprawdzający: Maciej Nowotka
*)


type memory = {
  tbl: (int Array.t, int) Hashtbl.t; (* hashtable zawierający stany i 
  liczbę czynności potrzebnych do osiągnięcia danego stanu *)
  q_states: (int Array.t) Queue.t; (* kolejka zawierająca stany do przerobienia *)
  final_state: int Array.t; (* stan końcowy [y1; y2; ...; yn]*)
  glass_capacities: int Array.t; (* pojemnosci szkalnek [x1, x2, ..., xn]*)
  n: int (* ilosc szkalnek *)
  }
;;

(* procedura sprawdzająca czy jest możliwe osiągnięcie stanu oczekiwanego *)
let is_possible xs ys = (* len xs > 0 *)
  let b = ref false in
  Array.iter2 (fun x y -> if x=y || y=0 then b := true) xs ys;

  let rec nwd a b =
    if a = 0 then b else nwd (b mod a) a
  in

  if !b then 
    begin
    let nwd_xs = Array.fold_left (fun acc x -> nwd acc x) xs.(0) xs in
    Array.iter (fun y -> if y mod nwd_xs <> 0 then b := false) ys;
    end;
  !b
;;

let pour_into mry s1 state = (* przelej z s1 do s2 *)
  let state_list = ref [] in
  for s2=0 to (mry.n-1) do
    if s1=s2 then () else
      let new_state = Array.copy state in

      let y1 = state.(s1) in
      let y2 = state.(s2) in
      let x2 = mry.glass_capacities.(s2) in

      new_state.(s1) <- max (y1+y2-x2) 0;
      new_state.(s2) <- min (y1+y2) x2;
      
      state_list := new_state::(!state_list);
  done;
  !state_list (* lista stanów osiągalnych po przelaniu s1 do dowolnej szklanki (z wyjątkiem s1) *)
;;

let pour_out s1 state = (* wylanie wody z s1 *)
  let new_state = Array.copy state in

  new_state.(s1) <- 0;
  new_state (* stan po wylaniu wody z s1 *)
;;

let pour mry s1 state = (* wlanie wody do s1 *)
  let new_state = Array.copy state in

  new_state.(s1) <- mry.glass_capacities.(s1);
  new_state (* stan po wlaniu wody do s1 *)
;;

let przelewanka szklanki = 
  let szklanki = Array.of_list (List.filter (fun x -> x <> (0, 0)) (Array.to_list szklanki)) in 

  let final_state = Array.map (fun (_, y) -> y) szklanki in
  let glass_capacities = Array.map (fun (x, _) -> x) szklanki in

  if Array.fold_left (+) 0 final_state = 0 || Array.length final_state = 0 then 0 else 
  if not (is_possible glass_capacities final_state) then -1 else

  let mry = 
    {
    tbl = Hashtbl.create 1000000; 
    q_states = Queue.create ();
    final_state = final_state; 
    glass_capacities = glass_capacities;
    n = Array.length szklanki
    }
  in

  let start_state = Array.make mry.n 0 in
  Queue.add start_state mry.q_states;  
  Hashtbl.add mry.tbl start_state 0;

  let sol = ref (-1) in

  while not (Queue.is_empty mry.q_states) && !sol = -1 do
    let state = Queue.take mry.q_states in
    let actions = Hashtbl.find mry.tbl state in

    (* funkcja zapisująca nowe stany do hashtable, dodająca nowe stany do kolejki 
    i zmieniająca wynik jeśli został osiągnięty stan oczekiwany*)
    let save new_state = 
      if not (Hashtbl.mem mry.tbl new_state) then 
        begin
        if new_state = mry.final_state then sol := actions+1;
        Queue.add new_state mry.q_states;
        Hashtbl.add mry.tbl new_state (actions+1)
        end;
    in
    
    for szk=0 to (mry.n-1) do
      let p_i = pour_into mry szk state in
      let p_o = pour_out szk state in
      let p = pour mry szk state in

      List.iter save (p_o::p::p_i);
    done;
  done;
  !sol
;;
