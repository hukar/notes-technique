# 05 Sequential II

## Bifs : Built In Function

Vous offre des fonction impossible à écrire en Erlang ou optimisées comme `lenght(List)`

Généralement ces fonctions sont écrites en C.

```erlang
1> date().
{2019,5,2}
2> time().
{16,28,37}

length(List)
size(Tuple)

atom_to_list(Atom)
list_to_tuple(List)
integer_to_list(2234)
tuple_to_list(Tuple)

```

```erlang
4> atom_to_list(titi).
"titi"
5> integer_to_list(456321).
"456321"
6> list_to_tuple("3456754").
{51,52,53,54,55,53,52} % la valeur ASCII de chaque chiffre
7> tuple_to_list({"hello",hello,78965,false,"titi",67,{minni,mickey,67}}).
["hello",hello,78965,false,"titi",67,{minni,mickey,67}]

8> hd([1,2,3]). % head
1
9> tl([1,2,3]). % tail
[2,3]
10> Tuple = {1,2,3}.
{1,2,3}
11> size(Tuple).
3
12> element(2, Tuple).
2
```

### Meta call : `apply()` BIF

```erlang
apply(Module, Function, Arguments) % Arguments set une liste
```

```erlang
15> lists:reverse([1,2,3]).
[3,2,1]
16> apply(lists,reverse,[[1,2,3]]). % attention aux doubles crochets
[3,2,1]
```

## Librairies de Erlang

### 4 vraiment importantes

- `io.erl`

- `file.erl`

- `lists.erl`

- `code.erl`  load, test manipulate code

  
## lists

  ```erlang
1> L1=[3,2,4,3].
[3,2,4,3]
2> L2=[25,43,7,3].
[25,43,7,3]
3> L1++L2.
[3,2,4,3,25,43,7,3]
4> lists:append(L1,L2).
[3,2,4,3,25,43,7,3]
5> lists:last(L2).
3
6> lists:delete(3,L1).
[2,4,3]
  ```



