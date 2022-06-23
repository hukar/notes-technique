# 03 Udemy Elixir Functionnal Thinking

## Pattern matching

`=` -> C, Python, Java, ...   **assignation**

`=` -> Erlang, Elixir   **pattern matching**

### mini programme de bingo

```bash
bingo = fn
    (88) -> "Bingo!"
    (_) -> "you lose"
end

card = 88

IO.puts bingo.(card)
```

```:ok:
iex(19)> c "./bingo.exs"
Bingo!
[]
```

si on tape `card = 33`

```:ok:
iex(20)> c "./bingo.exs"
you lose
[]
```



`_`  joker (= wildcard) **une** valeur qui ne peut être lu (= can't be read)

```bash
iex(15)> _ = 6
6
iex(16)> _ = 7
7
iex(17)> 7 = _
** (CompileError) iex:17: invalid use of _. "_" represents a value to be ignored in a pattern and cannot be used in expressions
```

```bash
iex(8)> [a, b, _] = [5, 7, "hello", :atomic_betty]
** (MatchError) no match of right hand side value: [5, 7, "hello", :atomic_betty]
# ici on voit que _ ne correspond que à une valeur et une seule
iex(8)> [a, b, _] = [5, 7, "hello"]
[5, 7, "hello"]
iex(10)> a
5
iex(11)> b
7
iex(12)> _
** (CompileError) iex:12: invalid use of _. "_" represents a value to be ignored in a pattern and cannot be used in expressions
```



### Assignation déstructurée

```bash
iex(1)> {a, b, c} = {:atomic, 'hello', "coco"}
{:atomic, 'hello', "coco"}
iex(2)> a
:atomic
iex(3)> b
'hello'
iex(4)> c
"coco"
```

Le **pattern matching** va essayer de faire correspondre les motifs des deux côtés du signe `=`

```bash
iex(5)> a = 1
1
iex(6)> b = 2
2
iex(7)> 1 = b
** (MatchError) no match of right hand side value: 2
iex(7)> 1 = a
1
```

### matching avec une liste

Une liste [] est une liste chainée.

```bash
iex(1)> list = [:anna, :betty]
[:anna, :betty]
iex(2)> [0|list]
[0, :anna, :betty]
iex(3)> list = [0|list]
[0, :anna, :betty]
iex(4)> list
[0, :anna, :betty]
```

assignation déstructurée avec une liste

```bash
iex(8)> [a, b, c] = [2, 4, 6]
[2, 4, 6]
iex(9)> a
2
```

Avec la deuxième écriture :

```:ok:
iex(10)> [head | tail] = [1,2,3,4]
[1, 2, 3, 4]
iex(11)> head
1
iex(12)> tail
[2, 3, 4]
```

tail = queue

exemple avec les fonction `hd` et `tl`  :

```bash
iex(12)> list = [:tete, :queue]
[:tete, :queue]
iex(13)> hd list
:tete
iex(14)> tl list
[:queue]
```



### pin operator `^`

`^var` correspond à la valeur de la variable

```:ok:
iex(13)> x = 1
1
iex(14)> x = 2
2
iex(15)> ^x = 3
** (MatchError) no match of right hand side value: 3
** equal to 2 = 3
```

`^` empêche l'assignation et à la place compare la valeur de `a` avec `3`

#### Comportement différent en erlang

`erl` pour avoir le REPL Erlang

```bash
Erlang/OTP 21 
Eshell V10.1.1  (abort with ^G)
1> A = 3.
3
2> A = 4.
** exception error: no match of right hand side value 4
3>
```



### Pattern Matching pour lire un fichier

```bash
iex(1)> {:ok, my_file} = File.open "hello.txtx"
** (MatchError) no match of right hand side value: {:error, :enoent}
iex(1)> {:ok, my_file} = File.open "hello.txt"
{:ok, #PID<0.105.0>}
iex(2)> IO.read my_file, :line
"hello\n"
iex(3)> IO.read my_file, :line
"\n"
iex(4)> IO.read my_file, :line
"I'm text about nothing\n"
iex(5)> IO.read my_file, :line
"Thank's (^-^)/"
iex(6)> File.close my_file
:ok
```

`File.open "file_name.bob"` ouvre un fichier et renvoie :

 `{:ok, lien vers le fichier}`

ou si problème : `{:error, :enoent}`

enoent = Error NO ENTry abréviation pour dire qu'il n'y a pas d'entrée

`IO.read my_file, :line` lit une ligne de `my_file`

`File.close my_file` ferme le fichier

### Pas de fonction à gauche d'une assignation

```bash
iex(1)> l = [:toto, :titi]
[:toto, :titi]
iex(6)> hd l = :toto
** (ArgumentError) argument error
    :erlang.hd(:toto)
iex(6)> :toto = hd l
:toto
iex(7)> l
[:toto, :titi]
```

## Données immuables

Protège la valeur d'une donnée lorsque celle-ci est utilisé dans plusieurs processus en même temps.