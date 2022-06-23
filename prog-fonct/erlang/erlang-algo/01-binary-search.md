# Recherche Dichotomic

```erlang
-export([dich/2]).

dich(Tuple, X) -> dichotomic(Tuple, X, 1, size(Tuple)).

dichotomic(Tuple, X, Begin, End) when Begin =<  End ->
    Position = (Begin + End) div 2,
    if
       element(Position, Tuple) == X -> {X, Position};
       element(Position, Tuple) > X -> dichotomic(Tuple, X, Begin, Position - 1);
       element(Position, Tuple) < X -> dichotomic(Tuple, X, Position + 1, End)
    end;
dichotomic(Tuple, X, Begin, End) when Begin > End -> {X,not_found}.
```

La recherche est faite avec un tuple car on peut atteidre un élément grâce à `element(Position, Tuple)`.

la première fonction est un alias raccourci, pour réduire aux deux paramètres obligatoires.

La condition de sortie est que `Begin` soit plus grand que `End`

```
10> T2={5,7,9,11,56,67,89}.  
{5,7,9,11,56,67,89}
11> sch:dich(T2,56).       
{56,5}
12> sch:dich(T2,9). 
{9,3}
13> sch:dich(T2,99).
{99,not_found}
```

## Avec une liste `lists:nth(Position, List)`

```erlang
% avec des list lists:nth(Position, List)

di(List, X) -> dicho(List, X, 1, length(List)).

dicho(List, X, Begin, End) when Begin =<  End ->
    Position = (Begin + End) div 2,
    PositionValue = lists:nth(Position, List),
    if
       PositionValue == X -> {X, Position};
       PositionValue > X -> dicho(List, X, Begin, Position - 1);
       PositionValue < X -> dicho(List, X, Position + 1, End)
    end;
dicho(List, X, Begin, End) when Begin > End -> {X,not_found}.
```

