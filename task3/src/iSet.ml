(* autor - Patryk Jedrzejczak
 * reviewer - Grzegorz Cichosz *)


(* [t] - typ wariantowy reprezentujacy zbior przedzialow liczb calkowitych jako
 * drzewo AVL
 * Empty - puste drzewo
 * Node (l, r, (x, y), n, h) - wezel o lewym poddrzewie [l] i prawym poddrzewie
 * [r] reprezentujacy przedzial liczb calkowitych [x, y] oraz bedacy korzeniem
 * drzewa o ilosci wszystkich liczb calkowitych [n] i wysokosci [h]
 * dla kazdego wezla Node zachodzi:
 * - roznica wysokosci poddrzew [l] i [r] nie jest wieksza niz 2
 * - przedzial [x, y] jest niepusty, tzn. [x <= y]
 * - wszystkie przedzialy w drzewie sa rozlaczne i niesasiednie
 * - wszystkie przedzialy w [l] sa mniejsze od [x], a wszystkie przedzialy w [r]
 *   sa wieksze od [y]
 * - jezeli ilosc liczb w drzewie jest wieksza niz max_int, to [n = max_int]
 * - wysokosc [h] drzewa jest rzedu logarytmu z liczby wezlow drzewa *)
type t =
  | Empty
  | Node of t * t * (int * int) * int * int

(* Zwraca wysokosc drzewa [s]. *)
let height s =
  match s with
  | Empty -> 0
  | Node (_, _, _, _, h) -> h

(* Zwraca ilosc wszystkich liczb calkowitych w zbiorze [s]. *)
let num_elem s =
  match s with
  | Empty -> 0
  | Node (_, _, _, n, _) -> n

(* Przyjmuje 2 nieujemne int'y [a], [b] i zwraca [min(a + b, max_int)]. *)
let safe_plus a b =
  if a >= max_int - b then max_int
  else a + b

(* Przyjmuje 2 int'y [a], [b] takie, ze a >= b i zwraca [min(a - b, max_int)].*)
let safe_minus a b =
  if b >= 0 || a < 0 then a - b
  else if a - max_int >= b then max_int
  else a - b

(* Przyjmuje 2 int'y [x], [y] takie, ze [x <= y] i zwraca ilosc liczb
 * calkowitych nalezacych do przedzialu [x, y] lub max_int jesli jest ich 
 * wiecej niz max_int. *)
let interval_len (x, y) =
  safe_plus (safe_minus y x) 1

(* Dla wezla o poddrzewach [l] i [r] oraz przedziale [xy], oblicza ilosc
 * liczb w zbiorze pilnujac, aby ta wartosc nie przekroczyla max_int. *)
let num_count l r xy =
  let lrn = safe_plus (num_elem l) (num_elem r) in
  let d = interval_len xy in
  safe_plus lrn d

(* Tworzy zbior zawierajacy jeden przedzial [xy]. *)
let leaf xy =
  Node (Empty, Empty, xy, interval_len xy, 1)

(* Tworzy wezel o podrzewach [l] i [r] oraz przedziale [xy]. *)
let make l r xy =
  Node (l, r, xy, num_count l r xy, max (height l) (height r) + 1)

(* Jesli dla drzewa o przedziale w korzeniu [xy], roznica miedzy wysokosciami
 * poddrzew korzenia [l] i [r] jest wieksza niz 2, to balansuje to drzewo tak,
 * zeby roznica miedzy wysokosciami poddrzew kazdego wezla byla niewieksza niz 2
 * oraz zeby byly spelnione wszystkie inne warunki poprawnosci drzewa. *)
let bal l r xy =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lr, lxy, _, _) ->
        if height ll >= height lr then make ll (make lr r xy) lxy
        else
          (match lr with
          | Node (lrl, lrr, lrxy, _, _) ->
              make (make ll lrl lxy) (make lrr r xy) lrxy
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rr, rxy, _, _) ->
        if height rr >= height rl then make (make l rl xy) rr rxy
        else
          (match rl with
          | Node (rll, rlr, rlxy, _, _) ->
              make (make l rll xy) (make rlr rr rxy) rlxy
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, r, xy, num_count l r xy, max hl hr + 1)

(* Dodaje do zbioru [s] niepusty przedzial [a, b] przy zalozeniu, ze jest to 
 * przedzial wiekszy lub mniejszy od wszystkich przedzialow w [s] oraz zaden
 * przedzial w [s] nie ma z nim elementow wspolnych lub sasiednich. *)  
let rec side_add (a, b) s =
  match s with
  | Empty -> leaf (a, b)
  | Node (l, r, (x, y), _, _) ->
      if b < x then bal (side_add (a, b) l) r (x, y)
      else bal l (side_add (a,b) r) (x, y)

(* Przyjmuje dwa zbiory [l] i [r] oraz przedzial [xy] wiekszy od wszystkich
 * przedzialow w [r], mniejszy od wszystkich przedzialow w [l] oraz
 * niesasiedni z zadnym z przedzialow w [l] i [r]. Zwraca zbior, powstaly
 * z polaczenia [l] i [r] oraz dodania wierzcholka zawierajacego [xy],
 * spelniajacy wszystkie warunki poprawnosci drzewa. *)
let rec join l r xy =
  match l, r with
  | Empty, _ -> side_add xy r
  | _, Empty -> side_add xy l
  | Node(ll, lr, lxy, _, lh), Node(rl, rr, rxy, _, rh) ->
      if lh > rh + 2 then bal ll (join lr r xy) lxy else
      if rh > lh + 2 then bal (join l rl xy) rr rxy else
        Node (l, r, xy, num_count l r xy, max lh rh + 1)

(* Zwrace krotke [(l, present, r)], gdzie
 * [l] jest zbiorem zawierajacym elementy [s] mniejsze od [a],
 * [r] jest ziorem zawierajacym elementy [s] wieksze od [a],
 * [present] ma wartosc [true] jesli [s] zawiera [a], a [false] jesli nie.*)
let rec split a = function
  | Empty -> (Empty, false, Empty)
  | Node (l, r, (x, y), _, _) ->
     if a >= x && a <= y then
       if a = x && a = y then (l, true, r)
       else if a = x && a < y then (l, true, side_add ((a + 1), y) r)
       else if a > x && a = y then (side_add (x, (a - 1)) l, true, r)
       else (side_add (x, (a - 1)) l, true, side_add ((a + 1), y) r)
     else if a < x then
       let (ll, pres, rl) = split a l in (ll, pres, join rl r (x, y))
     else
       let (lr, pres, rr) = split a r in (join l lr (x, y), pres, rr)

(* Zwraca najmniejszy wzgledem wartosci przedzial w zbiorze. Jesli zbior jest
 * pusty, zwraca (0, 0), co upraszcza implementacje funkcji add, ktora oczekuje
 * zwrocenia czegokolwiek przez min_interval.*)
let rec min_interval = function
  | Node (Empty, _, xy, _, _) -> xy
  | Node (l, _, _, _, _) -> min_interval l
  | Empty -> (0, 0)

(* Zwraca najwiekszy wzgledem wartosci przedzial w zbiorze. Jesli zbior jest
 * pusty, rowniez zwraca (0, 0) na potrzeby funkcji add. *)
let rec max_interval = function
  | Node (_, Empty, xy, _, _) -> xy
  | Node (_, r, _, _, _) -> max_interval r
  | Empty -> (0, 0)

(* Usuwa ze zbioru [s] liczby nalezace do przedzialu [a, b]. Mimo rekurencji
 * dziala w zlozonosci logarytmicznej, bo drugie wywolanie usuwa skrajnego
 * liscia, a wtedy [r = Empty], więc remove wykonuje się co najwyżej 2 razy.*)
let rec remove (a, b) s =
  let (l, _, ns) = split a s in
  let (_, _, r) = split b ns in
  if l = Empty then r else
  if r = Empty then l else
  let new_root = max_interval l in 
  join (remove new_root l) r new_root

(* Dodaje do zbioru [s] liczby nalezace do przedzialu [a, b]. *)
let add (a, b) s =
  let (l, _, ns) = split a s in
  let (_, _, r) = split b ns in
  let (lx, ly) = max_interval l in
  let (rx, ry) = min_interval r in
  if l <> Empty && r <> Empty && ly = a - 1 && rx = b + 1 then
    join (remove (lx, ly) l) (remove (rx, ry) r) (lx, ry)
  else if l <> Empty && ly = a - 1 then
    join (remove (lx, ly) l) r (lx, b)
  else if r <> Empty && rx = b + 1 then
    join l (remove (rx, ry) r) (a, ry)
  else
    join l r (a, b)

(* Zwraca pusty zbior. *)
let empty = Empty

(* Zwraca [true], jesli zbior [s] jest pusty, [false] w przeciwnym przypadku. *)
let is_empty s =
  s = Empty

(* Zwraca [true], jesli [a] jest elementem [s], [false] jesli nie jest. *)
let rec mem a s =
  match s with
  | Empty -> false
  | Node (l, r, (x, y), _, _) ->
      if a >= x && a <= y then true else
      if a < x then mem a l else
      mem a r

(* Wywoluje funkcje [f] na kazdym przedziale w zbiorze [s] w porzadku rosnacym
 * i zwraca [unit]. *)
let rec iter f = function
  | Empty -> ()
  | Node (l, r, xy, _, _) -> iter f l; f xy; iter f r 

(* Zwraca [(f xN ... (f x2 (f x1 a))...)], gdzie x1 ... xN to przedzialy
 * w zbiorze [s] w porzadku rosnacym. *)
let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, r, xy, _, _) ->
        loop (f xy (loop acc l)) r in
  loop acc s

(* Zwraca liste przedzialow w zbiorze [s] w porzadku rosnacym. *)
let elements s =
  List.rev (fold (fun xy acc -> xy :: acc) s [])

(* Zwraca ilosc liczb mniejszych od lub rownych [a] w zbiorze [s]. Jesli takich
 * liczb jest wiecej niz max_int, zwraca max_int. *)
let rec below a s =
  match s with
  | Empty -> 0
  | Node (l, r, (x, y), _, _) ->
      if a >= x && a <= y then
        safe_plus (interval_len (x, a)) (num_elem l)
      else if a > y then
        safe_plus (safe_plus (interval_len (x, y)) (num_elem l)) (below a r)
      else
        below a l