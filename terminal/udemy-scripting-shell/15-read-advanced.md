# 15 Read avancé

## `read -a`  stocker des données dans un tableau

```bash
#! /bin/bash

echo "entrez dans le tableau"
read -a mon_tableau

for i in {0..2}
do
    echo ${mon_tableau[$i]}
done
```

```bash
entrez dans le tableau
titi toto tata tutu
titi # mon_tableau[0]
toto # mon_tableau[1]
tata # mon_tableau[2] 
```

Si on dépasse l'index :

```bash
entrez dans le tableau
titi toto
titi
toto
			# juste rien ne s'affiche
```

## `mon_tableau[@]` ou `mon_tableau[*]`  afficher toutes les valeurs

```bash
#! /bin/bash

echo "entrez dans le tableau"
read -a mon_tableau

echo ${mon_tableau[@]}
echo ${mon_tableau[*]}
```

```
entrez dans le tableau
titi lulu bobo

titi lulu bobo
titi lulu bobo
```

## Variable par défaut de `read`

Si on utilise `read` sans spécifuer de variable `$REPLY` contient alors la dernière valeur introduite :

```bash
read # sans variable
titi toto tata

echo $REPLY
titi toto tata
```

## `readonly`  variable en lecture seule

Avec la commande `readonly`  une variable voie sa valeur fixée :

```bash
#! /bin/bash

read -p "entrez votre age " age

readonly age

age="pipi caca" # va générer l'erreur
```

```bash
./readonly.sh 
entrez votre age 67
./readonly.sh: line 7: age: readonly variable
```

