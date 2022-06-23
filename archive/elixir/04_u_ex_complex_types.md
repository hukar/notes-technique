# 04 Udemy Elixir Complex Types

## list

Les `list` sont des liste chaînées

### Concaténation

```bash
iex(2)> [:coco, :kitty] ++ [:toto]
[:coco, :kitty, :toto]
```

### Retirer un ou plusieurs élément(s)

```bash
iex(3)> ["toto", "titi", "toto"] -- ["toto"]
["titi", "toto"]
iex(4)> ["toto", "titi", "toto"] -- ["toto", "titi"]
["toto"]
```

### Attention à la transformation automatique en caractère

```bash
iex(1)> [6, 7, 8] -- [6]
'\a\b'
```

Pour résoudre on peut configurer iex :

```bash
iex(7)> IEx.configure inspect: [charlists: false]
:ok
iex(8)> [6, 7, 8] -- [6]
[7, 8]
```

Pour résoudre de manière permanente créer un fichier `~/.iex.exs` :

```elixir
# notre configuration
IEx.configure inspect: [charlists: false]
```

### Récupérer le premier élément

```bash
iex(2)> l = [:riri, :fifi, :loulou]
[:riri, :fifi, :loulou]
iex(3)> List.first l
:riri
```

### Opérateur de pointeur `|`

[1 -> [2, 3]] peut se lire 1 pointe vers la liste [2, 3] :

```bash
# en Elixir on écrit | pipe pour pointe
iex(4)> l = [2, 3]
[2, 3]
iex(5)> new_l = [1 | l]
[1, 2, 3]
iex(6)> new_l
[1, 2, 3]
```

De manière récursive on peut écrire :

```bash
iex(7)> [1 | [2 | [3 | []]]]
[1, 2, 3]
```

Où `[3 | []]` est égal à `[3]`

### Connaitre la taille d'une liste

Attention l'opération est en temps linéaire, cela peut donc être long pour une grande liste.

```bash
iex(7)> list = [2, 3]
[2, 3]
iex(8)> length list
2
```

### accéder à un élément par son index

```bash
iex(1)> list = [5, 7, 9]
[5, 7, 9]
iex(2)> Enum.at(list, 1)
7
```

On utilise `Enum.at`

Il existe aussi `Enum.fetch` et `Enume.fetch!`

La différence vient dans la gestion d'un index n'existant pas :

```bash
iex(3)> Enum.at(list, 99)
nil
iex(4)> Enum.fetch(list, 99)
:error
iex(5)> Enum.fetch!(list, 99)
** (Enum.OutOfBoundsError) out of bounds error
    (elixir) lib/enum.ex:863: Enum.fetch!/2
```



## Keyword list = liste associative

```bash
iex(1)> [{:a, 1}, {:b, 2}]
[a: 1, b: 2]
# c'est un raccourci d'écriture

iex(2)> [a: 1, b: 2] ++ [c: 3]
[a: 1, b: 2, c: 3]
# les opérations sont les mêmes qu'avec les listes

iex(3)> [d: 4 | [e: 5, f: 6]]
** (CompileError) iex:3: misplaced operator |/2
iex(3)> [{:d, 4} | [e: 5, f: 6]]
[d: 4, e: 5, f: 6]

iex(4)> l = [w: 56, x: 67, y: 78]
[w: 56, x: 67, y: 78]
iex(5)> [:h | :t] = l
** (MatchError) no match of right hand side value: [w: 56, x: 67, y: 78]
iex(5)> [head | tail] = l
[w: 56, x: 67, y: 78]
iex(6)> head
{:w, 56}
iex(7)> tail
[x: 67, y: 78]
```

### Accès à la keyword list

`Keyword.get` ou `list[:k]`

```elixir
iex(5)> keyword_list = [a: 45, b: 67, c: 28]
[a: 45, b: 67, c: 28]
iex(6)> Keyword.get(keyword_list, :b)
67
iex(7)> keyword_list[:c]
28
```

### Utilisation des keyword list pour passer des paramètres à une fonction

```elixir
iex(8)> cook = fn (heat, foods) -> Keyword.values(foods) |> Enum.map(&(heat <> &1)) end
#Function<12.128620087/2 in :erl_eval.expr/5>

iex(9)> cook.("fried ", [meat: "sausage", veg: "beans"])
["fried sausage", "fried beans"]
```

`<>`  => concaténer

```elixir
iex(1)> "hello " <> "coco"
"hello coco"
```



Si la keyword list est le dernier argument, on peut retirer les crochets :

```elixir
iex(16)> cook.("fried ", meat: "sausage", veg: "beans")
["fried sausage", "fried beans"]
```

`Keyword.values` 

```elixir
iex(11)> big_klist = [h: "raymond", f: "nana", h: "eric", h: "roger", f: "sara", h: "oscar"]

iex(12)> Keyword.values(big_klist)
["raymond", "nana", "eric", "roger", "sara", "oscar"]
```

Pour récupérer toutes les valeurs d'un clé particulière :

```elixir
iex(13)> Keyword.get(big_klist, :h)
"raymond"
# seulement la première occurrence de l'atom :h

iex(14)> Keyword.get_values(big_klist, :h)
["raymond", "eric", "roger", "oscar"]
# ici on récupère toutes les occurrences de l'atom :h
```

Occurrence: place occupé par un symbole

### Utilisation du if

```elixir
iex(17)> if true, do: :this, else: :that
:this
iex(18)> if false, do: :this, else: :that
:that
iex(19)> if(true, [do: :this, else: :that])
:this
```

On voit que `if` est une **macro** prenant deux paramètres, un booléen et une keyword list.

## Les map

Les `map` sont un type de dictionnaire

`Map` hérite (ou implémente) de Dict

Les `map` sont beaucoup plus performant que les `list`

```bash
iex(20)> h Dict

                                      Dict

deprecated: Use Map or Keyword modules instead

Generic API for dictionaries.

If you need a general dictionary, use the Map module. If you need to manipulate
keyword lists, use Keyword.

To convert maps into keywords and vice-versa, use the new function in the
respective modules.
```

###Syntaxe `map = %{key => value}`

```elixir
iex(21)> map = %{"nom" => "titi", "age" => 45, :taille => "1m67"}
%{:taille => "1m67", "age" => 45, "nom" => "titi"}
```

#### Une fonction peut être la clé

```elixir
iex(22)> m = %{fn -> "boom" end =>  "boom"}
%{#Function<20.128620087/0 in :erl_eval.expr/5> => "boom"}
iex(23)> Map.keys m
[#Function<20.128620087/0 in :erl_eval.expr/5>]
```

`Map.keys` pour récupérer les clés

Les maps n'acceptent qu'une seule valeur :

```elixir
iex(24)> %{:a => "renée", :a => "toto", :a => "anna"}
%{a: "anna"}
```

Les anciennes valeurs sont écrasées

Un map est beaucoup plus rapide qu'une keyword list

![map-list-elixir](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/map-list-elixir.png)

### Accès aux données du map

```elixir
iex(1)> map = %{:a => 3, :b => 5, :c => 9}
%{a: 3, b: 5, c: 9}
iex(2)> map[:b]
5
# accès avec les crochets map[:clé] 

iex(3)> map.b
5
# accès avec le point map.clé si et seulement si :clé est un atom

iex(4)> map[:z]
nil
# pas d'erreur avec la notation crochet

iex(5)> map.z
** (KeyError) key :z not found in: %{a: 3, b: 5, c: 9}
# Attention avec le point une erreur est levée
```

### écriture simplifié avec les atom (comme keyword list)

```elixir
iex(5)> %{a: 56, b: 89} === %{:a => 56, :b => 89}
true
```

### pattern matching

```elixir
iex(6)> [] = [a: 4, b: 6]
** (MatchError) no match of right hand side value: [a: 4, b: 6]
# ne fonctionne pas avec les keyword list
iex(6)> %{} = %{a: 4, b: 6}
%{a: 4, b: 6}
# fonctionne avec les map

iex(7)> %{a: a, b: 2} = %{a: 55, b: 2, c: 53}
%{a: 55, b: 2, c: 53}
iex(8)> a
55
```

### Module Map

#### accéder aux données

```elixir
iex(9)> Map.get(%{a: 6, b: 7, c: 78}, :c)
78
```

#### convertir un map en keyword list

```elixir
iex(10)> Map.to_list(%{a: 6, b: 7, c: 78})
[a: 6, b: 7, c: 78]
```

### updater (modifier) une valeur

```elixir
iex(11)> newmap = %{map | b: "sausage"}
%{a: 3, b: "sausage", c: 9}
iex(12)> newmap
%{a: 3, b: "sausage", c: 9}
# La valeur est modifié
iex(13)> map
%{a: 3, b: 5, c: 9}
# map reste inchangé

iex(14)> newmap = %{map | z: "sausage"}
** (KeyError) key :z not found in: %{a: 3, b: 5, c: 9}
# si la clé n'existe pas une erreur est levée
```



