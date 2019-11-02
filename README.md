[![Build Status](https://dev.azure.com/maciejmikolajek/maciejmikolajek/_apis/build/status/BinarySoftware.PseudoParser?branchName=master)](https://dev.azure.com/maciejmikolajek/maciejmikolajek/_build/latest?definitionId=1&branchName=master)

# PseudoParser
Pierwszy Parser pseudokodu.

### Użyte biblioteki
- org.enso.flexer - baza dla parsera
- org.enso.logger - logging management
- org.enso.unused 
- org.enso.data._ - funkcje ułatwiające pracę z danymi

### Funkcjonujące elementy AST
- AST.Var - Zmienne , z deklaracją typów
- AST.Func - Funkcja, z deklaracją argumentów
- AST.Comment - Komentarze w linii kodu
- AST.Elem.Newline - Znacznik początku nowej linii
- AST.Undefined - Cała reszra
