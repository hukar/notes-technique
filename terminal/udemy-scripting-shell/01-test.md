# 01 Test

## L'existence de fichiers

On peut effectuer plusieurs test sur les fichiers : `help test`

* -d 0 si dossier
* -f 0 si fichier
* -e 0 si existe
* -r 0 si readable
* -w 0 si writable
* -x 0 si executable
* -s 0 si existe et non vide

### Lire le retour `$?`

Pour lire le retour de la dernière commande `$?`

```bash
[ -e closet ] # est-ce que closet existe
kms: Desktop $ echo $?
0 # oui il existe
```

```bash
[ -f closet ]
echo $?
1 # ce n'est pas un fichier

[ -d closet ]
echo $?
0 # c'est un dossier
```

exemple de script :

```bash
#! /bin/bash

for i in `ls` # boucle dans le résultat de la commande ls
do
    echo "$i"

    if [ -f $i ]
    then
        echo "c'est un fichier"
    fi

    if [ -d $i ]
    then
        echo "c'est un dossier"
    fi
done
```

```
00-intro-variable.md
c'est un fichier
01-test.md
c'est un fichier
06-EOF-cat.md
c'est un fichier
07-conditions.md
c'est un fichier
scripts
c'est un dossier
```

## Test sur les chaînes de caractère

### une chaîne vide : `-z`

```bash
#! /bin/bash

read -p "Introduisez votre nom : " name
for i in {1..999}
do
    if [ -z $name ]
    then
        read -p "veuillez entrer votre nom deux fois !" name
    else
        echo "merci $name"
        break
    fi
done

echo "fin du programme"
```



#### ! respectez les espaces après et avant les crochets

```bash
PRENOM=""
[ -z $PRENOM ]
echo $?
0 # true

PRENOM="michel"
[ -z $PRENOM ]
echo $?
1 # false
```

### ! attention `true` => 0 et `false` => 1

Comparer deux chaînes : `=` 

```bash
PRENOM="nini"
NOM="nini"

[ $PRENOM = $NOM ] # attention aux espaces obligatoires
echo $?
0

[ $PRENOM != $NOM ]
echo $?
1
```

`==`  est aussi toléré par le bash mais la norme POSIX veut `=`

## Comparer des nombres

```bash
#! /bin/bash

NB1=5;
NB2=6;

# ==
[ $NB1 -eq $NB2 ];
echo $?; # 1

# !=
[ $NB1 -ne $NB2 ];
echo $?; # 0

# <
[ $NB1 -lt $NB2 ];
echo $?; # 0

# <=
[ $NB1 -le $NB2 ];
echo $?; # 0

# >
[ $NB1 -gt $NB2 ];
echo $?; #1

# >=
NB2=5;
[ $NB1 -ge $NB2 ];
echo $?; # 0
```

