# 01 Les listes

`|` cons operator

```erlang
1> [a,b,c] ++ [b,e].
[a,b,c,b,e]
2> [a,b,c] -- [b,d].
[a,c]
3> [a,b,c] -- [a,b] -- [c].
[c]
4> hd([1,2]).
1
5> tl([t,n,j,i]).
[n,j,i]
```

