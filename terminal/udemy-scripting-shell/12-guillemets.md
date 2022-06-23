# 12 Utilisation des guillemets

##  `$var`  vs `"$var"`  vs `${var}`  vs `"${var}"` 

### `$var` vs `"$var"`

Mettre des guillemets autour d'une variable dit au shell de traiter la variable comme un seul mot, même si la chaine contient des espaces.

```bash
sentence="hello monsieur kotto"

for word in $sentence # remplace par hello monsieur kotto
do
    echo $word # la boucle tourne trois fois
done
```

```
hello
monsieur
kotto
```

```bash
for word in "$sentence" # remplace par "hello monsieur kotto"
do  
    echo $word # la boucle tourne une fois
done
```

```
hello monsieur kotto
```

## Avec un tableau `$var`  `$var[@]`  `${var[@]}` 

```bash
foo=(a b c)
echo $foo
```

```
a
```

Si on n'indique pas d'index avec `[ ]`  , le tableau renvoie son premier élément.

```bash
echo ${foo}
```

```
a
```

Aucunes différences avec précédemment.

```bash
echo ${foo[@]}
```

```
a b c
```

Le tableau est bien affiché grâce au caractère `@` 

```bash
echo $foo[@]
```

```
a[@]
```

Affiche `a` ( par expansion de `$foo`) puis le littéral `[@]` 

## Subtilités entre `${var[@]}` et `"${var[@]}"` 

## `${var[@]}`

```bash
names=("michel aime margaret" "toto aime la maitresse")

for name in ${names[@]} # l'expansion égraine chaque mot
do
    echo $name #la boucle itère sept fois
done 
```

```
michel
aime
margaret
toto
aime
la
maitresse
```

## `"${var[@]}"`

```bash
names=("michel aime margaret" "toto aime la maitresse")

for name in "${names[@]}" # on conserve chaque phrase
do
    echo $name # il n'y a plus que deux itération
done 
```

```
michel aime margaret
toto aime la maitresse
```

C'est plus le résultat attendu.