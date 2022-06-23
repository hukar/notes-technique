# 03 Test Bash

## Syntaxe

```bash
[ expression_de_test ]
```

Les espaces avant et après l'expression, sont obligatoires.

La commande retourne 0 ou 1 (true ou false).

les options :

```bash
-e # (0) si le fichier existe
-d # (0) s'il sagit d'un répertoire
-s # si le fichier existe et qu'il nest pas vide
-w
-r
-x # (0) si le fichier a tel ou tel type de droit
```

Tous les test :

```bash
help test
```

```bash
-a FILE        # True if file exists.
# etc.
```

Il y a aussi des tests sur les chaînes et entre deux fichiers ou deux expressions.

```bash
hukar: script-shell $ [ -f php.sh ];echo $?;
0 # true php.sh existe
hukar: script-shell $ [ -f flute.sh ];echo $?;
1 # false flute.sh n'existe pas
```

`echo $?` va afficher le résultat de la dernière commande.

## Tests sur chaîne de caractères

```bash
#! /bin/bash

NOM="coco";
[ -z $NOM ];
echo $?;
```

```bash
1
```

```bash
#! /bin/bash

NOM="";
[ -z $NOM ];
echo $?;
```

```bash
0
```

`-z` test si la chaîne set vide.

`$?` affiche le résultat de la dernière commande lancée.

### Comparer deux chaînes de caractère :

```bash
#! /bin/bash

NOM="titi";
PRENOM="titi";
[ $NOM = $PRENOM ];

echo $?;
```

```bash
./bash-test.sh
0 # true
```

#### ! respecter les espaces avant et après le signe égal

De même pour différent `!=` :

```bash
[ $NOM != $PRENOM ];
echo $?;
```

```bash
./bash-test.sh
1
```

## Pour comparer des nombres

```bash
NB1=7
NB2=8
[ $NB1 -eq $NB2 ]; echo $?;
1
[ $NB1 -ne $NB2 ]; echo $?;
0
[ $NB1 -lt $NB2 ]; echo $?;
0
[ $NB1 -le $NB2 ]; echo $?;
0
[ $NB1 -gt $NB2 ]; echo $?;
1
[ $NB1 -ge $NB2 ]; echo $?;
1
```

