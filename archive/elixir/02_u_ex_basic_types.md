# 02 Udemy Elixir Basic Types

## Deux types de nombre : `float` et `integer`

### integer

base dix, octal, hexadecimal et binaire

On peut représenter les grands nombre avec un tiret bas :

`1_000_000_000_000_000` 

```bash
# représentation octal
iex(5)> 0o345
229

# représentation hexadécimal
iex(6)> 0xabc
2748

# représentation binaire
iex(7)> 0b101101
45
```

La division entière utilise deux fonction `div()` et `rem()`

rem = remainder = le reste de la division entière

```bash
iex(14)> div(9,2)
4
iex(15)> rem(9,4)
1
iex(16)> 9 / 2
4.5 # un float
```



### float

précision de 64 bits

Écriture avec exposant :

```bash
iex(9)> 123.0
123.0
iex(10)> 123.0e-2
1.23
iex(11)> 123.0e2
1.23e4
```

## Atoms

Les `atoms`  sont des `string` immuables.

Les `atoms` sont des **constantes** dont le nom est leur valeur propre.

Un atom commence par `:` ou une majuscule :

```bash
iex(6)> Atom
Atom
iex(7)> :atom
:atom
iex(8)> :atom == :"atom" # atoms are immutable string
true
```

Les `atoms` sont utilisés pour référencer des fonctions

## Booléens

Les booléens sont juste des atoms

```bash
iex(12)> :true == true
true
iex(13)> :false == false
true
iex(14)> false == 0
false
```

## String

interpolation `#{nom_var}`

```bash
iex(15)> name = "coco"
"coco"
iex(16)> IO.puts "hello #{name}"
hello coco
```

Simple guillemet et double guillemet :

simple guillemet = liste

double guillemet = string

### is_list

```bash
iex(18)> is_list 'hello'
true
iex(19)> is_list "hello"
false
```

### is_binary

```bash
iex(21)> is_binary "hello"
true
iex(22)> is_binary 'hello'
false
```

### byte_size

```bash
iex(29)> byte_size "o"
1
iex(30)> byte_size "œ"
2
iex(31)> byte_size "l'œuf de pépé" # 13 caractères
16
```

### String.length

```bash
iex(33)> String.length "l'œuf de pépé"
13
```

## Les fonctions

L'***arité*** est le nombre d'argument(s) d'une fonction

Une fonction anonyme:

commence par le mot `fn` et fini par le mot `end`

```bash
iex(2)> fn (p) -> "hello #{p}" end
#Function<6.128620087/1 in :erl_eval.expr/5>
```

On doit lui assigner un nom :

```bash
iex(3)> hello = fn (p) -> "coucou #{p}" end
#Function<6.128620087/1 in :erl_eval.expr/5>
iex(4)> hello.("gigi")
"coucou gigi"
```

Raccourci d'écriture :

```bash
iex(5)> salut = fn p -> "salut #{p}" end
#Function<6.128620087/1 in :erl_eval.expr/5>
iex(6)> salut.("titi")
"salut titi"
```

on peut enlever les parenthèse autour de l'argument

Et sans argument :

```bash
iex(7)> toto = fn -> "toto" end
#Function<20.128620087/0 in :erl_eval.expr/5>
iex(8)> toto.()
"toto"
```

### is_function

```bash
iex(11)> is_function toto
true
```

## Les tuples n-uplet

= tableau indexé immuable (=immutable)

on écrit un tuple comme ça :

`{"un", "deux", "trois"}`

### elem

Pour accéder à un membre d'un tuple on utilise son index dans la fonction `elem` :

```batch
iex(13)> elem({"titi","toto","tata","tonton"}, 2)
"tata"
```

Un tuple peut contenir des éléments de type divers :

```bash
iex(14)> t = {"un", 2, 4.5}
{"un", 2, 4.5}
iex(15)> elem(t, 0)
"un"
```

### tuple_size

```bahs
iex(16)> tuple_size t
3
```

### Retour de fonction

```bash
hello = fn () -> {:ok, 1, 2, 3} end
```

exemple réel d'une fonction renvoyant un tuple :

```bash
iex(20)> ls
hello.exs     hello.txt
iex(21)> File.open("./hello.txt")
{:ok, #PID<0.127.0>}
```

`File.open` renvoie un **tuple** avec l'atom `:ok` et un id pour le fichier

