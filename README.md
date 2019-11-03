[![Build Status](https://dev.azure.com/maciejmikolajek/maciejmikolajek/_apis/build/status/BinarySoftware.PseudoParser?branchName=master)](https://dev.azure.com/maciejmikolajek/maciejmikolajek/_build/latest?definitionId=1&branchName=master)

<p align="center">
<img src="https://github.com/BinarySoftware/PseudoLang/blob/wip/mm/ast_3/PseudoLang.png" style="margin: 0 auto;">
</p>
<h1 align="center">PseudoLang</h1>
Kiedy pseudokod przestaje być pseudo, wiesz że będzie ci łatwiej.

## Problem
Uczęszczając na zajęcia Podstaw Informatyki na wFiIS AGH zdałem sobię sprawę na przykładzie paru osób, jak bardzo sporym wyzwaniem może być napisanie lub zrozumienie pewnych algorytmów, jeśli w życiu wcześniej nie pisało się kodu. 
Problemem jest również to, iż wiele zadań wymaga zapisania algorytmu w pseudokodzie. Niestety, jako że jest to pseudokod, nie posiada on swojego interpretera czy debuggera, stąd taka osoba, nie znając innego języka, nie jest w stanie sprawdzić czy jej kod będzie funkcjonował poprawnie, bez podejścia do tablicy ( często kończącego się dwóją w dzienniku ) czy sprawdzenia swojej odpowiedzi z innym studentem, który również nie jest pewny cd. poprawności jego rozwiązania.

## Rozwiązanie
Postanowiłem pomóc takim delikwentom poprzez napisanie parsera dla naszego uczelnianego pseudokodu, oraz stworzenie transpilera owego kodu do Scali.

### Schemat działania:
1. Użytkownik wprowadza swój pseudokod
2. Następuje odpalenie parsera z podanym pseudokodem
3. Parser traversuje pseudokod, tokenizując go zgodnie z zapisanymi regułami, tworząc AST
4. Po zakończeniu pracy parsera, z powstałego AST generowany jest kod Scali, który później zostaje poddany kompilacji narzędziami scalowskimi.

W ten sposób użytkownik, nie znając syntaxu scali tworzy kod zrozumiały dla kompilatora scali i z nim operuje.
Powyższy schemat dla osób które mają rozeznanie w programowaniu może stawiać wiele pytań, między innymi, czy aby podwójne parsowanie kodu nie jest bardzo niewydajne? Otóż fakt, nie będzie to tak wydajne jak naturalny kod Scalowski, ale równie dobrze można powiedzieć, że najlepiej pisać kod bezpośrednio w assemblerze. 
**Good luck, have fun.**
Natomiast, dzięki użyciu szybkich jak piorun bibliotek które powstały dzięki pracy genialnego zespołu języka [luna/enso](https://github.com/luna/enso), parser będzie należał do jednego z najszybszych na rynku - celem jest uzyskanie co najmniej 7 mln znaków/sekundę

### Użyte biblioteki
- org.enso.flexer - baza dla parsera
- org.enso.logger - logging management
- org.enso.data._ - funkcje ułatwiające pracę z danymi

### Funkcjonujące elementy AST
- AST.Var - Zmienne , z deklaracją typów
- AST.Func - Funkcja, z deklaracją argumentów
- AST.Comment - Komentarze w linii kodu
- AST.Elem.Newline - Znacznik początku nowej linii
- AST.Undefined - Cała reszra
