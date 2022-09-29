# Sortowanie topologiczne

## Task

Write a function that sorts a graph topologically.

# Full description in polish

Sortowanie topologiczne polega na rozszerzeniu grafu skierowanego bez cykli (DAG-u) do porządku liniowego.

Mówiąc prościej, mając dany DAG należy przypisać wierzchołkom takie różne liczby naturalne (nadające kolejność tym wierzchołkom), żeby dla każdej krawędzi grafu jej źródło miało niższy numer niż jej cel.

Mówiąc jeszcze prościej, mając daną częściową informację o zależności np. czynności od siebie (np. buty wkładamy po skarpetkach, krawat po koszuli itp. ale kolejność wkładania skarpetek i koszuli może być dowolna) mamy wygenerować ścisłą kolejność wykonywania czynności (np. koszula, skarpetki, buty, krawat).

Konkretnie należy zaprogramować implementację [**topol.ml**](https://github.com/patjed41/WPF/blob/master/task5/src/topol.ml) załączonej specyfikacji [**topol.mli**](https://github.com/patjed41/WPF/blob/master/task5/src/topol.mli).

W implementacji można korzystać z modułu [**pMap**](https://github.com/patjed41/WPF/blob/master/task5/src/pMap.mli) (bardzo podobnego do [**pSet**](https://github.com/patjed41/WPF/blob/master/task3/src/pSet.mli) z zadania z drzewami AVL), którego specyfikacja i implementacja również są załączone. 

