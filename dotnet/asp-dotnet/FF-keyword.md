# Les mots clés du langage



## `as` et `is`

On est dans un contexte de `Type Checking` et de `type casting`.

`is` permet de tester le type d'un objet :

```cs
if(o is Cat) { ... }
```

`as` permet de `caster` un objet ou renvoie `null`.

```cs
o as cat
```

est équivalent à 

```cs
o is cat ? (Cat)o : (cat)null
```

