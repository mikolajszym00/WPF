(*
  Piszący: Mikołaj Szymański
  Sprawdzający: Jacek Spaliński
*)

(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type t =
| Empty
| Node of t * (int*int) * t * int * int
;;

type interval = 
| Ujemny 
| Zero 
| Dodatni 
;;

(* funkcja porównująca- określa ustawienie krańców zbioru (x, y) względem zbioru (k1, k2) *)
let cmp ((x, y):int*int) ((k1, k2):int*int) =
  let y_cond = y>=k1 && y<=k2 in
  if x < k1 then 
    match y with
    | _ when y<k1 -> (Ujemny, Ujemny) 
    | _ when y_cond -> (Ujemny, Zero) (* x jest na lewo od przedziału k, y jest pomiędzy k1 a k2 *)
    | _ when y>k2 -> (Ujemny, Dodatni)
    | _ -> assert false
  else 
    if x>=k1 && x<=k2 then if y_cond then (Zero, Zero) else (Zero, Dodatni)
    else (Dodatni, Dodatni)
;;

let between a b = if b - a + 1 <= 0 || (a, b) = (min_int, max_int) then max_int else b - a + 1;;

let height = function
| Node (_, _, _, h, _) -> h
| Empty -> 0
;;

let size = function
| Node (_, _, _, _, s) -> s
| Empty -> 0
;;

let make l k r = 
  let max_sum = size l + between (fst k) (snd k) + size r in (* wszystkie składniki sumy są większe lub równe od 0 *)
  Node (l, k, r, max (height l) (height r) + 1, if max_sum < 0 then max_int else max_sum)
;;

let empty = Empty
;;

let is_empty x = 
  x = Empty
;;

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r
;;

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found
;;

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"
;;

let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found
;;

let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_min_elt"
;;

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)
;;

let mem x t =
  let x = (x, x) in
  let rec loop = function
    | Node (l, k, r, _, _) ->
      let c = cmp x k in
      let pom =
      match c with
      | Zero, Zero -> true
      | Ujemny, Ujemny -> loop l
      | Dodatni, Dodatni -> loop r 
      | _ -> false
      in pom
    | Empty -> false in
  loop t
;;

let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop t
;;

let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc t
;;

let elements t = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] t
;;

(* zał. żaden element ze zbioru x nie należy do drzewa *)
let rec add_new x = function
  | Empty -> Node (Empty, x, Empty, 1, between (fst x) (snd x))
  | Node (l, v, r, _, _) ->
    if fst x < fst v then bal (add_new x l) v r
    else bal l v (add_new x r)
  ;;

let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_new v r
  | (_, Empty) -> add_new v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r
;;

let split x t =
  let x = (x, x) in
  let rec loop x = function
    | Empty -> (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
        let c = cmp x v in
        if c = (Zero, Zero) then (* fst v <= fst x = snd x <= snd v *)
          let nl = if fst x = fst v then l else add_new (fst v, fst x -1) l 
          and nr = if fst x = snd v then r else add_new (1+snd x, snd v) r 
          in (nl, true, nr)
        else if c = (Ujemny, Ujemny) then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else (* (Dodatni, Dodatni) *)
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in loop x t
;;

(* procedura łączy ze sobą dwa drzewa powstałe z podzielenia t ze względu na przedział x *)
let remove x t = 
  let (p, q) = x in
  let (l_part, _, _) = split p t 
  and (_, _, r_part) = split q t in

  merge l_part r_part (* max element l_part < min element r_part *)
;;

(* procedura łączy ze sobą dwa drzewa powstałe z usunięcia z t elementów zawartych w x. 
Następnie dodaje ona przedział x, korygując go o przedziały mogące się z nim stykać *)
let add x t = 
  let (p, q) = x in
  let (l_part, _, _) = split p t
  and (_, _, r_part) = split q t in

  let (l_part, corrected_p) = 
    match l_part with
    | Empty -> (l_part, p)
    | _     -> 
      let max_e = max_elt l_part in
      if snd max_e + 1 = p then (remove_max_elt l_part, fst max_e)
      else (l_part, p) 
  in

  let (r_part, corrected_q) = 
    match r_part with
    | Empty -> (r_part, q)
    | _     -> 
      let min_e = (min_elt r_part) in
        if fst min_e - 1 = q then (remove_min_elt r_part, snd min_e)
        else (r_part, q)
  in
  (* (max element l_part)+1 < corrected_p <= corrected_q < (min element r_part)-1 *)
  join l_part (corrected_p, corrected_q) r_part 
;;

let below (n:int) (t:t) =
  let lower_part = if n = max_int then t else remove (n+1, max_int) t in
  size lower_part
;;
