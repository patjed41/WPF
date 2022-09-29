(* autor - Patryk Jedrzejczak
 * reviewer - Mikolaj Pirog *)


(* Null - pusta kolejka
 * Node(l, v, r, h) - kolejka o lewym poddrzewie [l], priorytecie [v], prawym
 * poddrzewie [r] oraz dlugosci sciezki do skrajnie prawego liscia [h] *)
type 'a queue =
  | Null
  | Node of 'a queue * 'a * 'a queue * int

let empty = Null

exception Empty

(* Dla kolejki [q] zwraca dlugosc sciezki do skrajnie prawego liscia. *)
let get_height q =
  match q with
  | Null -> 0
  | Node(_, _, _, h) -> h

let rec join q1 q2 =
  match q1, q2 with
  | Null, _ -> q2
  | _, Null -> q1
  | Node(l1, v1, r1, h1), Node(_, v2, _, _) ->
    if v1 <= v2 then
      let q3 = join r1 q2 in
      if get_height l1 > get_height q3 then
        Node(l1, v1, q3, (get_height q3) + 1)
      else
        Node(q3, v1, l1, (get_height l1) + 1)
    else
      join q2 q1

let add v q =
  join (Node(Null, v, Null, 1)) q

let delete_min q =
  match q with
  | Null -> raise Empty
  | Node(l, v, r, _) -> (v, join l r)

let is_empty q =
  q = Null