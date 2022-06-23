# 03 liste et récursion

```erlang
1> [1|[2|3]].
[1,2|3] % impropre
2> [1|[2|[3]]].
[1,2,3] % propre
```

