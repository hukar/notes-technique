# 01 C : mon premier programme

## Différence entre clang, gcc, llvm

`LLVM` : Low Level Virtual Machine

`clang` ou `gcc` sont le **frontend** alors que `llvm` est le **backend**

`gcc` est déprécié sur mac

`Clang`  est le compulateur c/c++/objectiv-c officiel pour mac

`Llvm` est un **backend** compilateur pour l'optimisation et la production de code.

`Clang` et `gcc` sont des **frontend** compilateur, ils parsent le code et le transforme en une représentation compréhensible pour `llvm`

`clang` et `gcc` se trouve dans `/usr/bin`

`usr` : `user` Tous les binaires dont l'utilisateur a besoin, à l'origine il servait aussi de home `/usr/someone` maintenant c'est `/home/someone`

## Build

avec `cc`

```bash
cc ex1_mes.c -o ex1_mes
```

avec `make` on demande le fichier voulu, make examine le répertoire trouve un fichier `ex1_mes.c` et lance la commande `cc ex1_mes.c -o ex1_mes` par déduction.

```bash
make ex1_mes
```

## Différence entre deux fichiers

```bash
diff file1.txt file2.txt
```

## printf - breaking code

```c
printf("%d\n");
```

ce code va compiler et printf ira chercher ce qu'il trouve sur la pile `stack`.

```bash
-521394280 # par exemple
```

## Le debugger lldb

```bash
lldb ex1_mes
```

## Connaitre la version de C utilisée par le compilateur

```c
printf("%d\n", __STDC_VERSION__);
```

```bash
201112 # -> c11 dec. 2011
```

