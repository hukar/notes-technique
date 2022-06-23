# 13 stockage de données partie I

## ETS Erlang Term Storage

C'est le plus souvent un hashtable.

`hash_function(key)` détermine la position de la data dans la table.![Capture d’écran 2019-06-18 à 09.52.32](assets/Capture d’écran 2019-06-18 à 09.52.32.png)

Un Hashtable ne conserve pas l'ordre.

Si on a besoin de garder les données ordonnées, on utilisera un arbre binaire.

## `new`  et `delete` 

```erlang
3> TabId2 = ets:new(mytable, [named_table, private, bag]).
mytable
4> ets:delete(mytable).
true

5> TabId3 = ets:new(mytable, [bag]).                      
#Ref<0.1561168970.3791781890.206034>
6> ets:delete(TabId3).
true
```

### `ets:new(TableName, [option]) -> #ref`

Option:

- Type : `set (default)`, `ordered_set`, `bag`, `duplicate_bag`

- Access : `public`, `protected`, `private`  par défaut `protected`
- `{keypos, Pos}` : définit la position de la clé par défaut `Pos = 1`
- `named_table` : utilise le nom à la place de la ref

---

`set` , toutes les clés sont uniques, si on réutilise la même clé on écrase l'ancien élément.

`ordered_set` , clés uniques et données ordonnées

`bag` , Des clés dupliquées peuvent exister, mais tous les éléments sont uniques.

`duplicate_bag` ,  Des éléments dupliqués peuvent exister.

---

`public` la table peut être lue et écrite par tous le monde

`protected` la table peut être lue par tous le monde et écrite par son processus propriétaire

`private` la table ne peut être lue et écrite que par son processus propriétaire

---

### `ets:delete(#ref | TableName)`

Détruit la table

## `insert`  et `lookup`

```erlang
10> ets:new(countries, [set, named_table]).
countries
11> ets:insert(countries, {luigi, italy}).
true
12> ets:lookup(countries, dieter).
[]

%% set -> clé unique -> écrase élément précédent
13> ets:lookup(countries, luigi).
[{luigi,italy}]
14> ets:insert(countries, {luigi, austria}).
true
15> ets:lookup(countries, luigi).           
[{luigi,austria}]

%% delete/2 pour éffacer un élément
16> ets:delete(countries, luigi).
true
17> ets:lookup(countries, luigi).
[]
```

Un exmple de tuple trop petit :

```erlang
%% je place la clé en troisième position
23> ets:new(animals, [set,{keypos, 3},named_table]).
animals
%% le tuple a une taille de 6 >= (Pos = 3)
24> ets:insert(animals, {rept, 45, snake, green, yellow, black}).
true
25> ets:lookup(animals, snake).
[{rept,45,snake,green,yellow,black}]

%% maintenant le tuple a une taille de 2 < (Pos = 3)
26> ets:insert(animals, {felin, cat}).
** exception error: bad argument
     in function  ets:insert/2
        called as ets:insert(animals,{felin,cat})
```



### ets:insert(TableName, {Key, Element, ...})`

Le tuple doit être au moins de la taille de la position de la clé :

```erlang
18> ets:insert(countries, {raymond, peru, china, corea}).
true
19> ets:lookup(countries, raymond).
[{raymond,peru,china,corea}]

20> ets:insert(countries, {robert}).                     
true
21> ets:lookup(countries, robert).                       
[{robert}]
```



### `ets:lookup(TableName, Key)`

recherche un élément par sa clé

## Bag -> plusieurs élément par clé

```erlang
28> ets:new(cars, [bag,named_table]).
cars

29> ets:insert(cars, {wolswagen, blue}).
true
30> ets:insert(cars, {wolswagen, pink}).
true

31> ets:lookup(cars, wolswagen).
[{wolswagen,blue},{wolswagen,pink}]
```

## Table ets et processus propriétaire

Chaque table a un processus propriétaire. Si on crée la table ets à partir du shell, c'est celui-ci son propriétaire.

```erlang
1> self().
<0.138.0>
2> ets:new(animals, [named_table, bag]).
animals
3> ets:insert(animals, {insect, mouche}).
true
4> ets:i().
 id              name              type  size   mem      owner
 ----------------------------------------------------------------------------
 ac_tab          ac_tab            set   6      945      application_controller
 animals         animals           bag   1      318      <0.138.0>
```

#### ! lors d'une exception le processus du shell crash et un nouveau est relancé

```erlang
1> self().
<0.146.0>
2> a = b.
** exception error: no match of right hand side value b
3> self().
<0.149.0>
```

Du coup, lors d'une exception notre table est crée est détruite :

```erlang
1> ets:new(matable, [named_table,bag]).
matable
2> ets:insert(matable, {legume, patate}).
true
3> ets:tab2list(matable).
[{legume,patate}]
4> a + b.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  +/2
        called as a + b
5> ets:insert(matable, {fruit, orange}).
** exception error: bad argument
     in function  ets:insert/2
        called as ets:insert(matable,{fruit,orange})
```

On peut éviter cela en ***catchant*** l'exception :

```erlang
1> ets:new(chevaliers, [bag, named_table]).
chevaliers
2> ets:insert(chevaliers, {bronze, pegase}).
true
3> catch a = b.
{'EXIT',{{badmatch,b},
         [{erl_eval,expr,5,[{file,"erl_eval.erl"},{line,453}]},
          {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,437}]},
          {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
          {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
          {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}}
4> ets:tab2list(chevaliers).
[{bronze,pegase}]
```

### `ets:tab2list(matable)`

affiche la table sous forme de liste :

```erlang
1> ets:tab2list(fruits).
[{raisin,7},{pomme,5},{poire,8},{poire,3}]
```

### `ets:i()`

Affiche les informations de toutes les tables :

```erlang
7> ets:i().
 id              name              type  size   mem      owner
 ----------------------------------------------------------------------------
 ac_tab          ac_tab            set   6      945      application_controller
 fruits          fruits            bag   4      339      <0.173.0>
...
 logger          logger            set   6      1537     logger
 #Ref<0.1899635465.3470655491.16541> code              set   352    19774    code_server
...
 #Ref<0.1899635465.3470655491.17162> shell_records     ordered_set 0      145      <0.172.0>
ok
```

### `catch`

Permet qu'une exception ne crash pas le shell.

## Différence entre `bag` et `duplicate_bag`

```erlang
1> ets:new(pers1, [bag, named_table]).
pers1
2> ets:new(pers2, [duplicate_bag, named_table]).
pers2

5> ets:insert(pers1, {jojo, michiels}).
true
6> ets:insert(pers1, {jojo, michiels}).
true

9> ets:insert(pers2, {jojo, michiels}).
true
10> ets:insert(pers2, {jojo, michiels}).
true

11> ets:i().
 id              name              type  size   mem      owner
 ----------------------------------------------------------------------------
...
 pers1           pers1             bag   1      332      <0.184.0>
 pers2           pers2             duplicate_bag 2      339      <0.184.0>
...
12> ets:tab2list(pers1).
[{jojo,michiels}]
13> ets:tab2list(pers2).
[{jojo,michiels},{jojo,michiels}]
```

Dans un `bag`, un même couple clé/valeur inséré plusieurs fois ne sera qu'en un seul exemplaire.

Dans un `duplicate_bag`, chaque insertion du même couple est enregistrée.

## Différence entre `set` et `ordered_set`

Le `set` est un `hash` n'acceptant qu'une clé unique.

L'`ordered_set` lui, en plus est ordonné, c'est un arbre (`tree`).

```erlang
1> ets:new(band1, [set, named_table]).
band1
2> ets:new(band2, [ordered_set,named_table]).
band2

%% on insère les données pour band1
3> ets:insert(band1, {red, sarah}).
true
4> ets:insert(band1, {green, roger}).
true
5> ets:insert(band1, {yellow, john}). 
true
6> ets:insert(band1, {blue, els}).  
true

%% Puis exactement dans le même ordre pour band2
7> ets:insert(band2, {red, sarah}).  
true
8> ets:insert(band2, {green, roger}).
true
9> ets:insert(band2, {yellow, john}).
true
10> ets:insert(band2, {blue, els}).   
true

%% on remarque que pour le set les données sont dans un ordre arbitraire
%% même pas celui d'entrée des données
11> ets:tab2list(band1).
[{green,roger},{blue,els},{red,sarah},{yellow,john}]

%% brand2 quant à lui, est classé par ordre alphabétique.
12> ets:tab2list(band2).
[{blue,els},{green,roger},{red,sarah},{yellow,john}]
```

