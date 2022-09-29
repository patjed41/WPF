(* autor - Patryk Jedrzejczak *)
(* reviewer - Tomasz Kubica *)


(* Typ wartosc trzyma przyblizone wartosci jako przedzialy.
Wewnetrzny(a1, a2) reprezentuje przedzial [a1, a2],
Zewnetrzny(a1, a2) reprezentuje przedzial (-inf, a1] u [a2, inf),
Pusty reprezentuje przedzial (zbior) pusty. Rozwiazanie opiera sie na tym, ze
tylko takie przedzialy moga sie pojawic. *)
type wartosc = 
  | Wewnetrzny of float * float
  | Zewnetrzny of float * float
  | Pusty


(**************************** funkcje pomocnicze ******************************)


(* Funkcja popraw_wartosc : wartosc -> wartosc zapewnia, ze zawsze zachodzi
a1 <= a2, czyli poprawia wartosci zwracane przez inne funkcje. *)
let popraw_wartosc a =
  match a with
    | Pusty -> Pusty
    | Wewnetrzny(a1, a2) -> Wewnetrzny(min a1 a2, max a1 a2)
    | Zewnetrzny(a1, a2) -> Zewnetrzny(min a1 a2, max a1 a2)

(* Funkcja jeden_znak : wartosc -> wartosc zapewnia, ze wartosci reprezentujace 
przedzialy od liczby ujemnej do 0 pamietaja 0 jako -0. a dla przedzialow 
od 0 do liczby dodatniej jako 0. (niezmiennik), co ulatwia obliczenia. *)
let jeden_znak a =
  match a with
    | Pusty -> Pusty
    | Wewnetrzny(a1, a2) ->
      if classify_float a1 = FP_zero && a2 > 0. then Wewnetrzny(0., a2)
      else if classify_float a2 = FP_zero && a1 < 0. then Wewnetrzny(a1, (-0.))
      else a
    | Zewnetrzny(a1, a2) ->
      if classify_float a1 = FP_zero then Zewnetrzny((-0.), a2)
      else if classify_float a2 = FP_zero then Zewnetrzny(a1, 0.)
      else a

(* Funkcja polacz_wartosci: wartosc -> wartosc -> wartosc laczy 2 wartosci
wariantu Wewnetrzny (tylko dla Wewnetrzny dziala) i ponadto zaklada, ze jedna
z wartosci ma postac (-inf, x] a druga [y, inf) lub obie są zbiorem {0}. Takie
zalozenia to skutek implementacji funkcji plus i razy. *)
let rec polacz_wartosci a b =
  match (a, b) with
    | (Wewnetrzny(a1, a2), Wewnetrzny(b1, b2)) ->
      (* dwa zbiory {0} *)
      if classify_float a1 = FP_zero && classify_float b1 = FP_zero then a
      (* Niech (-inf, x] bedzie po lewej.*)
      else if a1 <> neg_infinity then polacz_wartosci b a
      else if a2 >= b1 then Wewnetrzny(neg_infinity, infinity)
      else Zewnetrzny(a2, b1)
    | _ -> raise (Invalid_argument "Argumenty musza byc typu Wewnetrzny")

(* Funckja wartosc_przeciwna : wartosc -> wartosc zamienia przedzial A na 
przeciwny B tak, ze x nalezy do B wtedy i tylko wtedy, gdy -x nalezy do A. *)
let wartosc_przeciwna a =
  match a with
    | Pusty -> Pusty
    | Wewnetrzny(a1, a2) -> Wewnetrzny((-.a2), (-.a1))
    | Zewnetrzny(a1, a2) -> Zewnetrzny((-.a2), (-.a1))

(* Funkcja wartosc_odwrotna : wartosc -> wartosc zamienia przedzial A na
odwrotny B tak, ze x nalezy do B wtedy i tylko wtedy, gdy 1/x nalezy do A. *)
let wartosc_odwrotna x =
  match x with
    | Pusty -> Pusty
    | Wewnetrzny(a, b) ->
      (* Odwrotnosc [0, 0] to zbior pusty. *)
      if classify_float a = FP_zero && classify_float b = FP_zero then
        Pusty
      else if a >= 0. || b <= 0. then
        popraw_wartosc (Wewnetrzny(1. /. a, 1. /. b))
      else 
        popraw_wartosc (Zewnetrzny(1. /. a, 1. /. b))
    | Zewnetrzny(a, b) -> 
      if a >= 0. || b <= 0. then 
        popraw_wartosc (Zewnetrzny(1. /. a, 1. /. b))
      else
        popraw_wartosc (Wewnetrzny(1. /. a, 1. /. b))

(* Funkcja mnoz_f : float -> float -> float mnozy dwa float'y z dodatkowym
zalozeniem, ze 0. *. inf = 0. oraz 0. *. -inf = 0. (wynikiem nie jest nan). *)
let mnoz_f x y =
  if (classify_float x = FP_zero && classify_float y = FP_infinite) ||
    (classify_float x = FP_infinite && classify_float y = FP_zero) then 0.
  else x *. y

(* Funkcja max_4 zwraca maximum z 4 liczb typu float. *)
let max_4 a b c d =
  max a (max b (max c d))

(* Funkcja min_4 zwraca minimum z 4 liczb typu float. *)
let min_4 a b c d =
  min a (min b (min c d))


(*************************** funkcje z interfejsu *****************************)


let wartosc_dokladnosc x p =
  let odstep = x *. p *. 0.01 in
  jeden_znak (popraw_wartosc (Wewnetrzny(x -. odstep, x +. odstep)))

let wartosc_od_do x y = jeden_znak (Wewnetrzny(x, y));;

let wartosc_dokladna x = Wewnetrzny(x, x)

let in_wartosc x y =
  match x with
    | Pusty -> false
    | Wewnetrzny(a, b) -> y >= a && y <= b
    | Zewnetrzny(a, b) -> y <= a || y >= b

let min_wartosc x =
  match x with
    | Pusty -> nan
    | Wewnetrzny(a, b) -> a
    | Zewnetrzny(a, b) -> neg_infinity

let max_wartosc x =
  match x with
    | Pusty -> nan
    | Wewnetrzny(a, b) -> b
    | Zewnetrzny(a, b) -> infinity

let sr_wartosc x =
  match x with
    | Pusty -> nan
    | Wewnetrzny(a, b) -> (a +. b) /. 2.
    | Zewnetrzny(a, b) -> nan

let rec plus a b =
  (* wynik to wynik dodawania wartosci *)
  let wynik =
    match (a, b) with
      (* Dwa przedzialy Wewnetrzny, czyli podstawowy przypadek, do ktorego
      sprowadzam bardziej skomplikowane. *)
      | (Wewnetrzny(a1, a2), Wewnetrzny(b1, b2)) ->
        Wewnetrzny(a1 +. b1, a2 +. b2)
      (* Przedzial Zewnetrzny(b1, b2) dziele na dwa przedzialy Wewnetrzny:
      Wewnetrzny(-inf, b1) i Wewnetrzny(b2, inf). Nastepnie oba osobno dodaje
      do przedzialu a, zeby na końcu polaczyc oba wynikowe przedzialy. *)
      | (Wewnetrzny(a1, a2), Zewnetrzny(b1, b2)) ->
        polacz_wartosci (plus a (Wewnetrzny(neg_infinity, b1)))
          (plus a (Wewnetrzny(b2, infinity)))
      (* Niech Wewnetrzny bedzie po lewej a Zewnetrzny po prawej.*)
      | (Zewnetrzny(a1, a2), Wewnetrzny(b1, b2)) -> plus b a
      (* Dla dwoch Zewnetrznych wszystko da sie osiagnac. *)
      | (Zewnetrzny(a1, a2), Zewnetrzny(b1, b2)) ->
        Wewnetrzny(neg_infinity, infinity)
      (* Jesli a lub b jest zbiorem pustym, to wynikiem jest zbior pusty. *)
      | _ -> Pusty
  (* utrzymanie niezmiennika z -0. jako koncem przedzialu i 0. jako poczatkiem*)
  in jeden_znak wynik

(* Odejmowanie przedzialu to po prostu dodawanie przedzialu przeciwnego. *)
let minus a b =
  plus a (wartosc_przeciwna b)

let rec razy a b =
  (* wynik mnozenia wartosci *)
  let wynik =
    match (a, b) with
      (* Dwa przedzialy Wewnetrzny, czyli to do czego chcemy sprowadzic bardziej
      skomplikowane przypadki. Krancami przedzialu bedacego wynikiem takiego 
      mnozenia beda najmniejsza i najwieksza z liczb a1 * b1, a1 * b2, a2 * b1,
      a2 * b2. *)
      | (Wewnetrzny(a1, a2), Wewnetrzny(b1, b2)) ->
        Wewnetrzny
          (min_4 (mnoz_f a1 b1) (mnoz_f a1 b2) (mnoz_f a2 b1) (mnoz_f a2 b2),
           max_4 (mnoz_f a1 b1) (mnoz_f a1 b2) (mnoz_f a2 b1) (mnoz_f a2 b2))
      (* Dziele Zewnetrzny na dwa Wewnetrzne analogicznie do dodawania.*)
      | (Wewnetrzny(a1, a2), Zewnetrzny(b1, b2)) ->
        polacz_wartosci (razy a (Wewnetrzny(neg_infinity, b1)))
          (razy a (Wewnetrzny(b2, infinity)))
      (* Niech Wewnetrzny bedzie po lewej a Zewnetrzny po prawej.*)
      | (Zewnetrzny(a1, a2), Wewnetrzny(b1, b2)) -> razy b a
      | (Zewnetrzny(a1, a2), Zewnetrzny(b1, b2)) ->
        (* Jesli choc jeden Zewnetrzny zawiera 0, wszystko da sie osiagnac. *)
        if a1 >= 0. || a2 <= 0. || b1 >= 0. || b2 <= 0. then
          Wewnetrzny(neg_infinity, infinity)
        (* Jesli nie, wynikiem jest nowy przedzial Zewnetrzny. *)
        else Zewnetrzny(max (a1 *. b2) (a2 *. b1), min (a1 *. b1) (a2 *. b2))
      (* Jesli a lub b jest zbiorem pustym, to wynikiem jest zbior pusty. *)
      | _ -> Pusty
  in jeden_znak wynik

(* Dzielenie przedzialu to po prostu mnozenie przedzialu odwrotnego. *)
let podzielic a b =
  razy a (wartosc_odwrotna b)