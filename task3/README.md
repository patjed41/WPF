# Modyfikacja drzew

## Task

Implement a libary of integer sets as AVL trees, where each node represents interval of values.

# Full description in polish

Zadanie polega na zmodyfikowaniu biblioteki zbiorów pojedynczych elementów zaimplementowanych jako pewien wariant drzew AVL (drzewa BST z wyważaniem). 

Dzięki wyważaniu wysokość drzewa jest zawsze rzędu logarytmu z liczby wierzchołków i dlatego wszystkie operacje wykonywane są w czasie logarytmicznym (nawet operacja `split`, ale to jest trochę mniej oczywiste: wynika z tego, że koszt `join` jest w istocie proporcjonalny do **różnicy** wysokości drzew, które łączymy. A ponieważ na `split` składa się ciąg operacji `join` na coraz wyższych drzewach, ich koszty sumują się do wysokości drzewa razy pewna stała).

Wynikiem modyfikacji ma być biblioteka zbiorów liczb całkowitych oparta o **przedziały**. Czyli elementami występującymi w drzewie muszą być przedziały, a nie pojedyncze liczby. Przedziały muszą być rozłączne i w dodatku, aby uniknąć niejednoznaczności reprezentacji, przedziały w drzewie nie mogą być "sąsiednie", czyli np. dwa przedziały $[1,\ldots,3]$ i $[4,\ldots,6]$ powinny być zastąpione przez jeden przedział $[1,\ldots,6]$. 

W naszej bibliotece dopuszczamy przedziały jednoelementowe, np. $[3,\ldots,3]$.

Wszystkie operacje (poza `fold`, `iter`, `elements` oraz `is_empty`) mają wykonywać się w czasie (zamortyzowanym) $O(log n)$, gdzie $n$ jest liczbą wierzchołków w drzewie.

Do zadania dołączona jest oryginalna [**specyfikacja**](https://github.com/patjed41/WPF/blob/master/task3/src/pSet.mli) i [**implementacja**](https://github.com/patjed41/WPF/blob/master/task3/src/pSet.ml) zbiorów (obie dostępne na licencji *GNU Lesser General Public License*) oraz [**specyfikacja**](https://github.com/patjed41/WPF/blob/master/task3/src/iSet.mli) zbiorów przedziałów, której implementację należy przesłać poprzez system moodle jako plik o nazwie [**iSet.ml**](https://github.com/patjed41/WPF/blob/master/task3/src/iSet.ml). 

Przy implementacji zwróć uwagę, jak zachowuje się Twój kod dla liczb równych bądź bliskich `max_int` (albo `min_int`). W szczególności konkretne wymaganie dotyczące tego aspektu dla funkcji below podane jest w specyfikacji ([**iSet.mli**](https://github.com/patjed41/WPF/blob/master/task3/src/iSet.mli)).

Jak zwykle implementacja powinna być udokumentowana; w szczególności należy wpisać w komentarzu niezmienniki dla używanych struktur danych oraz pre- i post-warunki wszystkich metod występujących w implementacji (zwłaszcza tych, których nazwy nie występują w specyfikacji). Warunki te mogą dotyczyć np. poprawności drzew, zakładanej różnicy wysokości drzew, itp.

---
Copyright of the task's description and resources: MIM UW.
