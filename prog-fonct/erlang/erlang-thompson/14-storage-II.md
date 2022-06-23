# 14 Stockage des données partie II

## Parcourir une table

### `ets:first(tname|tid)` retourne la première clé

### `ets:next(tname|tid, key)` retourne la clé après `key`

### `ets:last(tname|tid)` retourne la dernière clé

```erlang
1> ets:new(lib, [named_table,ordered_set]).
lib

2> ets:insert(lib, {rfa, tintin}),ets:insert(lib, {rfb, larousse}).
true
3> ets:insert(lib, {rfc, milou}),ets:insert(lib, {rfd, rambo}).    
true

4> ets:first(lib).
rfa
5> ets:last(lib). 
rfd
6> ets:next(lib, rfd).
'$end_of_table'
7> ets:next(lib, rfa).
rfb
```

Après le dernier élément, erlang renvoie  l'atom `'$end_of_table'`

## Utiliser le pattern matching

### `ets:match(tname|tid, {'$1', something, '_'})`

`'$0'`, `'$1'`, … Sont des atomes qui fonctionne comme variable (ordonnés) 

-> `[['$0', '$1', ...], ...]`

`'_'` est un atome joker = wildcard

Cela renvoie une liste de listes de résultat :

```erlang
1> ets:new(countries, [bag, named_table]).
countries

2> ets:insert(countries, {chris, ireland, programmer}). 
3> ets:insert(countries, {nancy, austria, cook}).      
4> ets:insert(countries, {royler, brazil, athletic}).
5> ets:insert(countries, {shaun, scotish, farmer}).  

6> ets:match(countries, {'$1', brazil, '_'}).
[[royler]]

7> ets:insert(countries, {georgia, brazil, secretary}).
8> ets:match(countries, {'$1', brazil, '_'}).          
[[georgia],[royler]]

9> ets:insert(countries, {roberta, brazil, programmer}).
10> ets:match(countries, {'$1','$0', programmer}).
[[brazil,roberta],[ireland,chris]]
```

### `ets:match_object`

### `ets:match_delete`

#### ! Il faut utiliser les fonctions Bifs avec prudence car elle stop les autres processus

Préférer `first`, `next` et `last`

## `ets:select`

```erlang
20> ets:select(group, [{{'$1', '$2', '$3'}, [{'==', '$3', orange}], [['$2', '$1']]}]).           
[[patate,micheline],[salade,micheline],[patate,roger]]
```

On peut utiliser la transformation d'une fonction en *'match specification'*

### `ets:fun2ms`

```erlang
21> MS = ets:fun2ms(fun({Name, Vegetable, Color}) when Color == orange -> [Vegetable, Name] end).
[{{'$1','$2','$3'},[{'==','$3',orange}],[['$2','$1']]}]

22> ets:select(group, MS).
```

![Capture d’écran 2019-06-27 à 15.25.40](assets/Capture d’écran 2019-06-27 à 15.25.40.png)

## Autres manipulation de table

### `ets:tab2file`  écrire une table dans un fichier

###`ets:file2tab`  -> `{ok, tab}` opération inverse

### `ets:tab2list` Transforme une table en liste

```erlang
7> ets:tab2file(tab, 'tab.ets').
ok

14> {_,Mytab} = ets:file2tab('tab.ets').
{ok,tab}

15> ets:tab2list(Mytab).
[{choco,carrot}, {vanilla,potatoe}, {vanilla,potatoe}]
```

### `ets:info(tab)`

```erlang
16> ets:info(Mytab).
[{id,#Ref<0.1736591529.2847801347.249264>},
 {read_concurrency,false},
 {write_concurrency,false},
 {compressed,false},
 {memory,339},
 {owner,<0.228.0>},
 {heir,none},
 {name,tab},
 {size,4},
 {node,nonode@nohost},
 {named_table,true},
 {type,duplicate_bag},
 {keypos,1},
 {protection,protected}]
```

Donne des infos sur une table en particulier

### `ets:i()`

Donne un résumé de toutes les tables

## Autres

### `dets` : disk erlang term storage

C'est une version sauvegardée sur le disque de la table

### ! une table dépend de la vie de son processus

### ! une table n'est pas *'garbage collecté'*