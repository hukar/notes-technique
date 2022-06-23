# 08 faire des calculs

## avec `expr`

```bash
nb=3
nb2=4

expr $nb + $nb2
7
```



## avec `$(( ... ))`

```bash
nb=3
nb2=4

echo $((nb + nb2))
7
```

## Script décompte

```bash
#! /bin/bash

read -p "veuillez entrer un nombre : " nb

while [ $nb -ne 0 ]
do
    echo "le compteur est à : $nb"
    nb=`expr $nb - 1`
done
```

## Chiffre aléatoire `$RANDOM`

`$RANDOM`  est une fonction qui renvoie un entier pseudo aléatoire entre 0 - 32767.

```bash
# une valeur entre 0 et n - 1

expr $RANDOM % n

# une valeur entre 1 et n

expr `expr $RANDOM % n`+ 1

# une valeur entre min et max
echo $(( ($RANDOM % (max - min + 1) ) + min ))
```

#### !  ` $[ … ]` est remplacé par `$(( … ))`

