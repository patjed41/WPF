(* autor - Patryk Jędrzejczak 
 * reviewer - Julia Podrażka *)

type point = float * float

type kartka = point -> int

let eps = 0.000000001

(* Porównywanie float'ów z dokładnością do [eps]. *)
let (=) a b = abs_float (a -. b) <= eps
let (<=) a b = a -. eps < b
let (>=) a b = a +. eps > b

(* Przesuwa początek wektora o początku (x1, y1) i końcu (x2, y2) do (0, 0)
 * i zwraca nowy koniec tego wektora. *)
let wektor (x1, y1) (x2, y2) = (x2 -. x1, y2 -. y1)

(* Zwraca iloczyn wektorowy dwóch wektorów. *)
let ilo_wekt (x1, y1) (x2, y2) = x1 *. y2 -. y1 *. x2

(* Zwraca odbicie symetryczne punktu (xs, ys) względem prostej przechodzącej
 * przez punkty (x1, y1) i (x2, y2). *)
let odbicie (x1, y1) (x2, y2) (xs, ys) =
  if x1 = x2 then (2. *. x1 -. xs, ys)
  else if y1 = y2 then (xs, 2. *. y1 -. ys)
  else 
    let a = (y1 -. y2) /. (x1 -. x2) in
    let b = y1 -. a *. x1 in
    let a' = -.(1. /. a) in
    let b' = ys -. a' *. xs in
    let xp = (b' -. b) /. (a -. a') in
    let yp = a *. xp +. b in
    (2. *. xp -. xs, 2. *. yp -. ys)

(* Zwraca 1, jeśli punkt (xs, ys) leży wewnątrz prostokąta o lewym dolnym rogu
 * (x1, y1) i prawym górnym rogu (x2, y2), a 0 w przeciwnym przypadku. *)
let prostokat (x1, y1) (x2, y2) (xs, ys) =
  if xs >= x1 && xs <= x2 && ys >= y1 && ys <= y2 then 1
  else 0

(* Zwraca 1, jeśli punkt (xs, ys) leży wewnątrz okręgu o środku (x, y)
 * i promieniu [r], a 0 w przeciwnym przypadku. *)
let kolko (x, y) r (xs, ys) =
  let square q = q *. q in
  if square (xs -. x) +. square (ys -. y) <= square r then 1
  else 0 

let zloz p1 p2 k ps =
  let strona = ilo_wekt (wektor p1 ps) (wektor p1 p2) in
  if strona = 0. then k ps
  else if strona >= 0. then 0
  else k ps + k (odbicie p1 p2 ps)

let skladaj proste k =
  List.fold_right (fun (p1, p2) -> zloz p1 p2) (List.rev proste) k