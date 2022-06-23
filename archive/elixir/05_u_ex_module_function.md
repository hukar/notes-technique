# 05 Udemy Elixir Modules et Fonctions

## Modules

```elixir
defmodule Greet do
    # nothing   
end
IO.puts Greet
```

```bash
$ elixir hello.exs
Elixir.Greet
```

Tous les modules sont englobés par le module Elixir.

On peut imbriquer les modules :

```elixir
defmodule Welcome do
    defmodule Greet do
        # nothing
    end
end
```

Ou bien écrit comme ça avec un point :

```elixir
defmodule Welcome.Greet do
    # nothing
end
```

#### best practice : avoir un module par fichier

Si on compile un fichier avec plusieurs module on aura plusieurs fichiers .beam :

```elixir
defmodule Welcome do
    defmodule Greet do
        # nothing
    end
end
IO.puts Welcome.Greet
```

```bash
elixirc hello.exs
```

Donne deux fichiers `.beam`

`Elixir.Welcome.beam` et `Elixir.Welcome.Greet.beam`



avec ce code on a un seul fichier :

```elixir
defmodule Welcome.Greet do
    def hello do
        "hello"
    end
end
IO.puts Welcome.Greet
```

Si le `.beam`  existe, il est chargé dans le `repl` `iex`.

## Fonctions nommées

Elles doivent être crées dans un module.

```elixir
# Named functions
# they must be created in a module
defmodule Cook do
#multiline function
    def boil(food) do
        f = "boil #{food}"
        String.upcase(f)
    end
end

IO.inspect Cook.boil("steak")
```

```:wavy_dash:
"BOIL STEAK"
```

Fonction en une ligne

```elixir
# Named functions
# they must be created in a module
defmodule Cook do
    #multiline function
    def boil(food) do
        f = "boil #{food}"
        String.upcase(f)
    end
    #singleline function
    def sausage, do: boil("sausage")
end

IO.inspect Cook.boil("steak")
IO.inspect Cook.sausage
```

```:wavy_dash:
"BOIL STEAK"
"BOIL SAUSAGE"
```

## Fonctions publics ou privées

On utilise `defp`

```elixir
defmodule Veg do
    # public function
    def public_cook(meat, veg), do: private_cook("grill", meat) <> private_cook("boil", veg)
    # private function
    defp private_cook(cook, food) do
        "#{cook} #{food} "
    end
end

IO.inspect Veg.public_cook "lambs", "brocoli"
```

```:wavy_dash:
elixir veg.exs
"grill lambs boil brocoli "
```

Par convention, une fonction privée commence par underscore `_` :

```elixir
defmodule Veg do
    # public function
    def cook(meat, veg), do: _cook("grill", meat) <> _cook("boil", veg)
    # private function
    defp _cook(cook, food) do
        "#{cook} #{food} "
    end
end

IO.inspect Veg.cook "lambs", "brocoli"
IO.inspect Veg._cook "chicken", "courget"
```

```:wavy_dash:
elixir veg.exs
"grill lambs boil brocoli "
** (UndefinedFunctionError) function Veg._cook/2 is undefined or private. Did you mean one of:
      * cook/2
    Veg._cook("chicken", "courget")
```



## Fonction avec plusieurs cas (clauses)

```elixir
defmodule Cook do
    ## pattern matching allows functions
    # to have multiple clauses
    def cook, do: cook("fry", "sausage")
    def cook(0), do: cook("bake", "banana")
    def cook(1), do: cook("mix", "persil")
    def cook(meat), do: cook("grill", meat)
    def cook(type, food), do: "#{type} #{food}"
end

IO.inspect Cook.cook
IO.inspect Cook.cook(0)
IO.inspect Cook.cook(1)
IO.inspect Cook.cook("beef")
IO.inspect Cook.cook("boil","fish")
```

```:wavy_dash:
$ elixir multicook.exs

"fry sausage"
"bake banana"
"mix persil"
"grill beef"
"boil fish"
```

remarque: le nom du fichier et le nom du module peuvent être différent.



## Cas avec condition : guard clauses

On utilise le mot clé `when` avec `==` ou les `built-in function` `is_XXX`

```elixir
defmodule Cook2 do
    def cook(food) when food == 0 do
        "Nothing cooking"
    end
    def cook(food) when is_integer(food), do: "food is a number #{food}"
    def cook(food) when is_atom(food), do: "food is atomic"
    def cook(food) when is_binary(food), do: cook("boil", food)
    def cook(type, food), do: "#{type} #{food}"
end

# les parenthèses ne sont pas obligatoires autour des paramètres de la fonction
IO.inspect Cook2.cook 0
IO.inspect Cook2.cook 99
IO.inspect Cook2.cook :titi
IO.inspect Cook2.cook "rosbeef"
```

```:wavy_dash:
$ elixir cook_02.exs
"Nothing cooking"
"food is a number 99"
"food is atomic"
"boil rosbeef"
```

remarque: binary == string

## Arguments par défaut

La valeur par défaut d'un argument est définie par deux back-slash `\\`

```elixir
#function can have default arguments
defmodule Cook3 do
    def cook(type \\"grill", food), do: "#{type} #{food}"
end

IO.inspect Cook3.cook "cake"
```

```:wavy_dash:
$ elixir cook_03.exs
"grill cake"
```

### Avec des cas multiples, on doit avoir une entête de fonction (function head)

```elixir
## Default arguments with multiple clauses need
# a function head (blank function)
defmodule Cook3 do
    def cook(food, type \\"boiled", veg \\"beans")
    def cook(food, type, veg), do: "Tasty #{food} with #{type} #{veg}"
end

IO.inspect Cook3.cook "brussels sprouts"
```

```:wavy_dash:
$ elixir cook_03.exs
"Tasty brussels sprouts with boiled beans"
```

### Des fonctions qui se chevauchent : overlapping functions

```elixir
## Compiler warns of overlapping functions
defmodule Cook3 do
    def cook(food, type \\"boiled", veg \\"beans")
    def cook(food, type), do: "overlapping function"
    def cook(food, type, veg), do: "Tasty #{food} with #{type} #{veg}"
end

IO.inspect Cook3.cook "brussels sprouts"
```

```:wavy_dash:
$ elixir cook_03.exs
warning: variable "food" is unused
  cook_03.exs:5

warning: variable "type" is unused
  cook_03.exs:5

** (CompileError) cook_03.exs:5: def cook/2 conflicts with defaults from cook/3
    cook_03.exs:5: (module)
```

La fonction avec deux arguments ne sera pas atteinte avec les paramètres par default

`fun "un", "deux" => fun "un", "deux", "default argument"` 

### Pipe operator `|>`

On peut composer avec les fonctions

1.

```elixir
IO.puts String.upcase(String.reverse("pipistrelle"))
```

2.

```elixir
IO.puts String.upcase("pipistrelle") |> String.reverse
```

3.

```
IO.puts "pipistrelle" 
    |> String.reverse
    |> String.upcase
```

On obtient toujours le même résultat :

```:wavy_dash:
ELLERTSIPIP
```



## Captures et références

Les fonctions sont représentées 'nom/arité' (name/arity)

L'arité étant le nombre d'argument.

### Capture operator `&`

```bash
iex(1)> &is_nil/1
#Function<6.128620087/1 in :erl_eval.expr/5>
iex(2)> &hd/1
&:erlang.hd/1
```

Elixir retourne une fonction et Erlang retourne un atom

Approche classique :

```bash
iex(3)> fn x -> x + 1 end
#Function<6.128620087/1 in :erl_eval.expr/5>
iex(4)> inc = fn x -> x + 1 end
#Function<6.128620087/1 in :erl_eval.expr/5>
iex(5)> inc.(1)
2
```

Transformation d'une fonction anonyme avec le **capture operator** :

```elixir
 fn x -> x + 1 end   # explicit anonymous function
 (  x -> x + 1   )   # remove do/end and add brackets
&(       x + 1   )   # remove body and add capture operator
&(      $1 + 1   )   # replace x with capture arguments
```

```bash
iex(6)> i = &(&1 + 1)
#Function<6.128620087/1 in :erl_eval.expr/5>
iex(7)> i.(1)
2
```

Avec des types complexes :

### List

```bash
iex(8)> a = &[&1 + 1, &2]
#Function<12.128620087/2 in :erl_eval.expr/5>
iex(9)> a.(5,6)
[6, 6]
```

### Keyword list

```bash
iex(12)> c = &[name: &1 <> " kiki", speed: &2*5/7]
#Function<12.128620087/2 in :erl_eval.expr/5>
iex(13)> c.("robert", 45)
[name: "robert kiki", speed: 32.142857142857146]
```

rappel : `<>` operator de concaténation

### Map

```bash
iex(10)> b = &%{one: &2, two: &1}
#Function<12.128620087/2 in :erl_eval.expr/5>
iex(11)> b.(3, 4)
%{one: 4, two: 3}
```

