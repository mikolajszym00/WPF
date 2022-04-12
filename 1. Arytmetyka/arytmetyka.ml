(*
  Piszący: Mikołaj Szymański
  Sprawdzający: Tomasz Surowiec
*)

open List
open Float

exception InvalidInput of string

(* 
Operator *. zwracał niepoprawną wartość np. dla -0.0 *. neg_infinity (nan zamiast 0.0).
W związku z tym został został zastąpiony przez operator *!
*)
let ( *! ) a b =
  match a, b with
  | 0.0, _ | _, 0.0 -> 0.0
  | _ -> a *. b

(*
Empty określa pusty zbiór;
Single to reprezentacja przedziału domkniętego: [x, y];
Double reprezentuje sumę przedziałow domkniętych jednostronnie: (neg_inginity, x] U [y, infinity]);
*)
type wartosc =
  | Empty
  | Single of (float * float)
  | Double of ((float * float) * (float * float))

(* Konstruktory *)

let wartosc_dokladnosc (x : float) (p : float) =
  if p < 0.0
  then raise (InvalidInput "p < 0.0")
  else (
     let a = x *! (1.0 -. p /. 100.0)
    and b = x *! (1.0 +. p /. 100.0) in
    Single (min a b, max a b)
  )
;;

let wartosc_od_do (x : float) (y : float) =
  if x > y
  then raise (InvalidInput "x > y")
  else Single (x, y)
;;

let wartosc_dokladna (x : float) = Single (x, x)

(* Selektory *)

let in_wartosc (w : wartosc) (x : float) =
  let pom_in (w1, w2) x = if x >= w1 then if x <= w2 then true else false else false in
  match w with
  | Empty -> false
  | Single w -> pom_in w x
  | Double (w1, w2) -> if pom_in w1 x || pom_in w2 x then true else false
;;

let min_wartosc (w : wartosc) =
  match w with
  | Empty -> nan
  | Single (w1, _) -> w1
  | Double w -> neg_infinity
;;

let max_wartosc (w : wartosc) =
  match w with
  | Empty -> nan
  | Single (_, w2) -> w2
  | Double _ -> infinity
;;

let sr_wartosc (w : wartosc) =
  match w with
  | Single (w1, w2) -> (w1 +. w2) /. 2.
  | Double _ | Empty -> nan
;;

(* Modyfikatory *)

let plus (a : wartosc) (b : wartosc) =
  match a, b with
  | Empty, _ | _, Empty -> Empty
  | Single (a1, a2), Single (b1, b2) -> Single (a1 +. b1, a2 +. b2)
  | Double _, Double _ -> Single (neg_infinity, infinity)
  | Single a, Double (c1, c2) | Double (c1, c2), Single a ->
    let l1, l2 = snd c1 +. snd a, fst c2 +. fst a in
    if l1 >= l2
    then Single (neg_infinity, infinity)
    else Double ((neg_infinity, l1), (l2, infinity))
;;

(* 
Odejmownie spełniane jest poprzez sumę wartości, której drugi składnik ma przeciwny znak
*)
let minus (a : wartosc) (b : wartosc) =
  match a, b with
  | Empty, _ | _, Empty -> Empty
  | a, Single (b1, b2) -> plus a (Single (-1. *! b2, -1. *! b1))
  | a, Double ((c1, c2), (d1, d2)) -> plus a (Double ((c1, -1. *! d1), (-1. *! c2, d2)))
;;

let razy (a : wartosc) (b : wartosc) =
  match a, b with
  | Empty, _ | _, Empty -> Empty
  | Single (0., 0.), _ | _, Single (0., 0.) -> Single (0., 0.)
  | Single a, Single b ->
    let s_list = [ fst a *! fst b; snd a *! snd b; fst a *! snd b; snd a *! fst b ] in
    let s_min =
      fold_left (fun a h -> if is_nan h then a else min a h) (hd s_list) (tl s_list)
    in
    let s_max =
      fold_left (fun a h -> if is_nan h then a else max a h) (hd s_list) (tl s_list)
    in
    Single (s_min, s_max)
  | Double (c1, c2), Double (d1, d2) ->
    if in_wartosc (Double (c1, c2)) 0.0 || in_wartosc (Double (d1, d2)) 0.0
    then Single (neg_infinity, infinity)
    else (
      let l1, l2 =
        ( max (snd c1 *! fst d2) (fst c2 *! snd d1)
        , min (snd c1 *! snd d1) (fst c2 *! fst d2) )
      in
      Double ((neg_infinity, l1), (l2, infinity)))
  | Single (a1, a2), Double (c1, c2) | Double (c1, c2), Single (a1, a2) ->
    if in_wartosc (Single (a1, a2)) 0.0
      then Single (neg_infinity, infinity)
    else (
      let pom new1 new2 =
      let l1, l2 = new1 *! snd c1, new2 *! fst c2 in
      if l1 > l2
      then Double ((neg_infinity, l2), (l1, infinity))
      else Double ((neg_infinity, l1), (l2, infinity)) in

      (*
      znak_c1: precyzuje znak lewego brzegu podwójnego 
      przedziału jeśli równa się on zeru
      (w takim wypadku znak ten będzie dodatni);

      a1 *! (znak_c1) > 0.0: nierówność 
      określająca znak a1 w zależności 
      od znaku znak_c1;

      in_wartosc (Double (c1, c2)) 0.0:
      określa przynależność zera 
      do podwójnego przedziału;
      *)
      let znak_c1 = if (snd c1) = 0.0 then 1. else snd c1 in 
      match (a1 *! (znak_c1) > 0.0, in_wartosc (Double (c1, c2)) 0.0) with
      | true, true ->  pom a2 a1
      | false, true ->  pom a1 a2
      | true, false ->  pom a2 a2
      | false, false ->  pom a1 a1
    )
;;

(* 
Dzielenie jest odwrotnością mnożenia, więc po odpowiednim 
odwróceniu dzielników może wywołać się procedura razy
*)

let podzielic a b =
  match a, b with
  | Empty, _ | _, Empty -> Empty
  | _, Single (0.0, 0.0) -> Empty
  | a, Single (b1, b2) ->
    if b1 < -0.0 && b2 > 0.0
    then razy a (Double ((neg_infinity, 1. /. b1), (1. /. b2, infinity)))
    else 
      (* 
      Przedziały zawierające na końcach 0.0 mogą dawać 
      przy odwracaniu niepoprawne wartości, dlatego trzeba je poprawić:
      (neg_infinity, -0.0) - poprawne
      (0.0, infinity) - poprawne      
      *)
      (
      let new_b1 = if b1 = -0.0 then 0.0 else b1 in
      let new_b2 = if b2 = 0.0 then -0.0 else b2 in
      razy a (Single (1. /. new_b2, 1. /. new_b1)))
  | a, Double ((c1, c2), (d1, d2)) ->
    if c2 > 0.0 || d1 < -0.0
    then razy a (Double ((neg_infinity, 1. /. d1), (1. /. c2, infinity)))
    else (
      let new_c2 = if c2 = 0.0 then -0.0 else c2 in
      let new_d1 = if d1 = -0.0 then 0.0 else d1 in
      razy a (Single (1. /. new_c2, 1. /. new_d1)))
;;
