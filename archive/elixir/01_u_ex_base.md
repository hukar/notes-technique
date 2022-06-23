# 01 Udemy Elixir Base

## iex elixir REPL

REPL = Read Eval Print Loop

Hello World avec le REPL dans le terminal

```bash
> iex
> IO.puts("hello world!")
```

```bash
hello world!
```

#### débloquer une erreur

Parfois lorsqu'on fait une faute de syntaxe on se retrouve dans une situation comme celle-la :

```elixir
iex(9)> mm["yo]
...(9)> mm["yo"]
...(9)> 
```

On peut sortir de là en tapant `ctrl + g` puis `i` (interrupt) puis `enter` puis `c` (connect) puis `enter`

## Dans un fichier

On crée le fichier `hello_world.exs`

```elixir
IO.puts("hello coco!")
```

Dans le terminal :

```bash
> elixir hello_world.exs
hello coco!
```

## Types de fichier .ex .exs

`.ex` => fichiers compilés

`.exs` => **e**li**x**ir **s**cript

## snake_case naming

minuscule plus tiret bas :

`mon_nom_de_fichier_en_snake_case`

## Commentaires

```elixir
# 01 hello elixir

# IO.puts("hello kitty")

IO.puts "hello kiki"
```

`#` : commentaires

On peut retirer les parenthèses de `IO.puts`

## iex interactive elixir => REPL

```bash
hukar: elixir-udemy $ iex
Erlang/OTP 21 [erts-10.1.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]

Interactive Elixir (1.7.4) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> String.downcase "TITI"
"titi"
iex(2)>
```

`String.downcase` pour mettre en minuscule

## iex commande de base

```bash
h # help

# example usable
• c/1            - compiles a file
• c/2            - compiles a file and writes bytecode to the given path
• cd/1           - changes the current directory
• clear/0        - clears the screen
• h/0            - prints this help message
• h/1            - prints help for the given module, function or macro
• i/0            - prints information about the last value
• i/1            - prints information about the given term
• ls/0           - lists the contents of the current directory
• ls/1           - lists the contents of the specified directory
• pwd/0          - prints the current working directory
```

`clear` nettoie la console

`h` affiche l'aide

`respawn` relance le REPL

`h IO.puts` h fonction => aide détaillée

```bash
  @spec puts(device(), chardata() | String.Chars.t()) :: :ok

Writes item to the given device, similar to write/2, but adds a newline at the
end.

By default, the device is the standard output. It returns :ok if it succeeds.

## Examples
    IO.puts "Hello World!"
    #=> Hello World!

    IO.puts :stderr, "error"
    #=> error
```

## Avoir la liste des fonctions d'un module

module. + `tab`

```bash
iex(6)> String. # push tab button
Break                   Casing                  Chars
Normalizer              Tokenizer               Unicode
at/2                   ...
```

## Compiler

```bash
c "hello.exs"
```

## Configurer iex

```bash
h IEx.configure
```

Changer la couleur de l'affichage du résultat :

```bash
IEx.configure colors: [eval_result: [:green,:bright]]
```

