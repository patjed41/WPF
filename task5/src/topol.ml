(* autor - Patryk Jędrzejczak 
 * reviewer - Tomasz Ziębowicz *)

(* Rozwiązanie polega na zbudowaniu grafu i wyznaczeniu odwróconego porządku
 * postorder w drzewach DFS tego grafu. W takim porządku wierczhołki grafu są
 * poprawnie posortowane topologicznie, o ile graf jest acykliczny. Złożoność
 * czasowa rozwiązania to O(n + m), gdzie [n] oznacza liczbę różnych 
 * wierzchołków w wejściowej liście, a [m] liczbę krawędzi. Pomijam czas
 * wykonywania operacji z modułu PMap. *)

open PMap

exception Cykliczne

(* Typ służący do odróżniania już odwiedzonych wierzchołków w algorytmie DFS.
 * In - weszliśmy do wierzchołka, ale jeszcze z niego nie wyszliśmy
 * Out - wyszliśmy z wierzchołka *)
type visited = In | Out

(* make_graph:
 * Funkcja przekształcająca wejście w graf trzymany w mapie. Pod kluczem [v]
 * znajduje się lista wierzchołków, do których prowadzi krawędź z [v]. *)

(* dfs:
 * Funkcja wykonuje algorytm DFS. Przyjmuje i zwraca zmnienioną parę
 * [(vis, post)], gdzie [vis] to mapa z odwiedzonymi już wierzchołkami a [post]
 * to zbudowany dotychczas fragment wynikowej listy z kolejnością postorder. *)

(* walk:
 * Wywołuje po kolei dfs na wszystkich nieodwiedzonych wierzchołkach. *)

let topol input =
  let make_graph l =
    let step graph v neigh =
      let new_vert g u =
        if mem u g then g
        else add u [] g
      in
      let graph2 = List.fold_left new_vert graph (v :: neigh) in
      add v (neigh @ find v graph2) graph2
    in
    List.fold_left (fun graph (v, neigh) -> step graph v neigh) empty l
  in
  let rec dfs graph (vis, post) v =
    let dfs_rec (vis', post') u =
      if not (mem u vis') then dfs graph (vis', post') u
      else if find u vis' = In then raise Cykliczne
      else (vis', post')
    in
    let vis2, post2 =
      List.fold_left dfs_rec (add v In vis, post) (find v graph)
    in
    (add v Out vis2, v :: post2)
  in
  let walk graph =
    let run_dfs v _ (vis', post') =
      if not (mem v vis') then dfs graph (vis', post') v
      else (vis', post')
    in
    foldi run_dfs graph (empty, [])
  in
  snd (walk (make_graph input))