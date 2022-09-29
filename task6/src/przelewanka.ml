(* autor - Patryk Jędrzejczak
 * reviewer - Patrycja Krzyna *)


(* gcd p q zwraca NWD(p, q), przy czym [p] i [q] mogą być równe 0 *)
let rec gcd p q =
  if q = 0 then p
  else if p mod q = 0 then q
  else gcd q (p mod q)

(* bfs - przechodzi w szerz graf stanów, umieszczając minimalną liczbę kroków 
 * potrzebną do uzyskania odwiedzanych stanów w tablicy hashującej [dist];
 * przerywa się, gdy dojdzie do oczekiwanego stanu y1, y2, ..., yn *)

(* check - sprawdza warunki konieczne istnienia rozwiązania, czyli podzielność
 * każdego yi przez NWD(x1, x2, ..., xn) oraz istnienie przynajmniej jednej
 * pustej lub pełnej szklanki pośród y1, y2, ..., yn *)

(* push u steps - jeśli stan [u] nie jest odwiedzony, dodaje stan [u] do
 * kolejki [q] i ustawia jego odległość w [dist] *)
let przelewanka tab =
  let n = Array.length tab in
  let x = Array.init n (fun i -> fst tab.(i)) in
  let y = Array.init n (fun i -> snd tab.(i)) in
  let dist = Hashtbl.create n in
  let solution = ref (-1) in
  let bfs start =
    if start = y then solution := 0;
    let q = Queue.create () in
    Queue.add start q;
    Hashtbl.add dist start 0;
    let push u steps =
      if not (Hashtbl.mem dist u) then begin
        if u = y then solution := steps + 1;
        Queue.add u q;
        Hashtbl.add dist u (steps + 1)
      end
    in
    while not (Queue.is_empty q) && !solution = -1 do
      let v = Queue.take q in
      let steps = Hashtbl.find dist v in
      for i = 0 to n - 1 do
        (* dolanie wody do pełna (u1), wylanie całej wody (u2) *)
        let u1 = Array.copy v and u2 = Array.copy v in
        u1.(i) <- x.(i);
        push u1 steps;
        u2.(i) <- 0;
        push u2 steps;
        (* przelanie wody do innej szklanki *)
        for j = 0 to n - 1 do
          if i <> j then begin
            let u = Array.copy v in
            u.(i) <- max (v.(i) - (x.(j) - v.(j))) 0;
            u.(j) <- min (v.(i) + v.(j)) x.(j);
            push u steps;
          end
        done
      done
    done
  in
  let check () =
    let gcd_x = Array.fold_left (fun a e -> gcd a e) 0 x in
    let divisible = ref true in
    let empty_or_full = ref false in
    for i = 0 to n - 1 do
      if gcd_x <> 0 && y.(i) mod gcd_x <> 0 then divisible := false;
      if y.(i) = 0 || y.(i) = x.(i) then empty_or_full := true
    done;
    n = 0 || (!divisible && !empty_or_full)
  in
  if check () then bfs (Array.make n 0);
  !solution
