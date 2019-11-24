<p align="center">
<img src="https://github.com/BinarySoftware/PseudoLang/blob/master/PseudoLang.png" style="margin: 0 auto;">
</p>
<h1 align="center">PseudoLang</h1>
Kiedy pseudokod przestaje być pseudo, wiesz że będzie ci łatwiej.

## Problem
Uczęszczając na zajęcia Podstaw Informatyki na wFiIS AGH zdałem sobię sprawę 
na przykładzie paru osób, jak bardzo sporym wyzwaniem może być napisanie lub 
zrozumienie pewnych algorytmów, jeśli w życiu wcześniej nie pisało się kodu. 
Problemem jest również to, iż wiele zadań wymaga zapisania algorytmu w 
pseudokodzie. Niestety, jako że jest to pseudokod, nie posiada on swojego 
interpretera czy debuggera, stąd taka osoba, nie znając innego języka, 
nie jest w stanie sprawdzić czy jej kod będzie funkcjonował poprawnie, 
bez podejścia do tablicy ( często kończącego się dwóją w dzienniku ) czy 
sprawdzenia swojej odpowiedzi z innym studentem, który również nie jest pewny 
poprawności jego rozwiązania.

## Rozwiązanie
Postanowiłem pomóc takim delikwentom poprzez napisanie parsera dla naszego 
uczelnianego pseudokodu, oraz stworzenie transpilera owego kodu do Pythona.

### Schemat działania + postęp prac :
1. Użytkownik wprowadza swój pseudokod 
   `Wprowadzono FileManager`
2. Następuje odpalenie parsera z podanym pseudokodem 
   `Wprowadzono szkielet uruchomieniowy dla parsera`
3. Parser traversuje pseudokod, tokenizując go zgodnie z zapisanymi regułami, 
   tworząc AST `Praca nad Parserem została ukończona`
4. Po zakończeniu pracy parsera, z powstałego AST generowany jest kod 
   Pythona, który później zostaje poddany kompilacji narzędziami 
   pythonowskimi. `Praca nad Transpilerem została ukończona`
5. `W przyszłości: Zmiana Transpilera na Interpreter napisany od 
    podstaw - GraalVM + Truffle`

W ten sposób użytkownik, nie znając syntaxu Pythona tworzy kod zrozumiały dla 
kompilatora Pythona i z nim operuje, co więcej, ma dostęp do wszystkich bibliotek,
a zatem funkcji napisanych dla Pythona
Powyższy schemat dla osób które mają rozeznanie w programowaniu może stawiać 
wiele pytań, między innymi, 
czy aby podwójne parsowanie kodu nie jest bardzo niewydajne? 
Otóż fakt, nie będzie to tak wydajne jak naturalny kod Pythonowski, 
ale równie dobrze można powiedzieć, że najlepiej pisać kod bezpośrednio w 
assemblerze. 
**Good luck, have fun.**
Natomiast, dzięki użyciu szybkich jak piorun bibliotek które powstały dzięki 
pracy genialnego zespołu języka [luna/enso](https://github.com/luna/enso), 
parser będzie należał do jednego z najszybszych na rynku - celem jest uzyskanie 
średniej co najmniej 2 mln znaków/sekundę `Uzyskano 2,6 - Ref. Bench`

### Użyte biblioteki
| Biblioteka | Opis |
| ---------- | ---- |
| org.enso.flexer | Podstawowa biblioteka, baza dla parsera, wylicza NFA i DFA, kontroluje stany |
| org.enso.logger | Logger z funkcją tracingu |
| org.enso.data._ | Funkcje ułatwiające pracę z danymi |
| org.enso.debug  | Funkcje ułatwiające debugowanie pracy parsera, pretty-printer dla AST |

### Funkcjonujące elementy AST
| Element | Opis |
| ---------- | ---- |
| AST           | Służy do przechowania AST tworzonego po zakończeniu parsowania tekstu |
| AST.Elem      | podstawowy element AST, szkielet najnizszego poziomu do dziedziczenia przez wyzsze elementy |
| AST.Empty     | Pusty element AST, nic nie wnosi, tylko do użytku w czasie parsowania, by nie tworzyc Option[] |
| AST.Var       | Zmienne, z deklaracją typów |
| AST.Func      | Funkcja, z deklaracją argumentów |
| AST.Comment   | Komentarze w linii kodu |
| AST.Newline   | Znacznik początku nowej linii |
| AST.Spacing   | odstępy między elementami |
| AST.Block     | Fragment kodu z wcięciem od linii bazowej |
| AST.Opr       | Operatory arytmetyczne, logiczne, przypisywania, typowania |
| AST.Undefined | Cała reszra elementów, które nie mogły zostać sparsowane na poprawne elementy AST |
| AST.Return    | Element zwracający wartość z funkcji |
| AST.Array     | Element trzymający tablice/listę |
| AST.Parens    | Element trzymający nawiasy `(`, - argumenty i warunki, `[` - elementy tablicy |
| AST.If        | Sprawdzanie warunkowe "Jeżeli", dozwolone jest zagnieżdzanie |
| AST.If.ThenCond | Element przechowujący blok, wykonywany jeśli Prawda |
| AST.If.ElseCond | Element przechowujący blok, wykonywany jeśli Fałsz |
| AST.For         | Pętla `For` |
| AST.While       | Pętla `While` |
| AST.DoWhile     | Pętla `Do ... While` |
| AST.RepeatUntil | Pętla Repeat ... Until, syntactic sugar dla `Do ... While Not(Warunek)` |

### Wyniki benchmarków Parsera
| Typ | Wynik - Znaki/S |
| --- | --------------- |
|Zmienne   | 4'879'420  |
|Funkcje   | 3'060'727  |
|Operatory | 910'222    |
|Bloki     |  1'575'384 |
| **Średnia** | 2'606'438 |
