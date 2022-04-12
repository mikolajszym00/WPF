(*
  Piszący: Mikołaj Szymański
  Sprawdzający: Tomasz Surowiec
*)

type point = float*float
;;

type kartka = point -> int
;;

type side = Left | On | Right
;;

let prostokat (p1: point) (p2: point): kartka =
  fun (x, y) ->
    if fst p1 <= x && fst p2 >= x &&
       snd p1 <= y && snd p2 >= y then 1 else 0 
;;

let squere = fun x -> x*.x
;;

let kolko (p1: point) (r: float): kartka = 
  fun (x, y) ->
    if squere (fst p1 -. x) +. squere (snd p1 -. y) <= squere r then 1 else 0 
;;

(* określa, z której strony względem złożenia przebito kartkę*)
let which_side (pin: point) (p1: point) (p2: point) =
  (* liczy wyznacznik macierzy (jej pierwsza kolumna to kolumna jedynek) *)
  let det = fst pin *. snd p1 +. fst p1 *. snd p2 +. fst p2 *. snd pin 
  -. fst pin *. snd p2 -. fst p1 *. snd pin -. fst p2 *. snd p1 in

  let det = if abs_float det < 0.00000001 then 0. else det in

  if det > 0. then Left 
  else if det < 0. then Right else On
;;

(* wyznacza punkt symetryczny do pin względem prostej wyznaczonej przez p1 i p2 *)
let symmetrical (pin: point) (p1: point) (p2: point) = 
  if fst p1 = fst p2 then (2. *. fst p1 -. fst pin, snd pin)
  else 
    if snd p1 = snd p2 then (fst pin, 2. *. snd p1 -. snd pin)
    else

  let a = (snd p1 -. snd p2)/. (fst p1 -. fst p2) in 
  
  let b_pin = snd pin +. (1./.a) *. fst pin 
  and b_p = snd p2 -. a *. fst p2 in
  
  let x = (b_pin -. b_p) /. ((1./.a) +. a) in
  let y = (-1./.a) *. x +. b_pin in
  (2. *. x -. fst pin, 2. *. y -. snd pin)

let zloz (p1: point) (p2: point) (sheet:kartka): kartka =
  fun pin ->
    match which_side pin p1 p2 with
    | Left -> 
      let symm_pin = symmetrical pin p1 p2 in
      sheet pin + sheet symm_pin
    | On -> sheet pin
    | Right -> 0
;;

let skladaj (line_list: (point*point) list) (sheet: kartka): kartka =
  let f = fun s line -> zloz (fst line) (snd line) s in 
  List.fold_left f sheet line_list
;;
