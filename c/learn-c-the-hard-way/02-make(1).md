# 02 commande `make`

`makefile` est un fichier de configuration pour la commande `make`

```bash
# C FLAG Setting
CFLAGS=-Wall -g

all: clean ex1

clean:
	rm -f ex1
```

```bash
make ex1
#result :
cc -Wall -g    ex1.c   -o ex1
```

Sans makefile :

```bash
make ex1
#result :
cc ex1.c -o ex1
```

### Ajouter des paramètre directement ans le terminal  :

En déclarant une variable avant l'exécution, celle-ci n'existe que pour le programme la suivant.

`hello.sh`

```bash
#!/bin/bash

echo $MON_NOM
```

Puis dans la console

```bash
kar : ex02 $ ./bonjour.sh 

kar : ex02 $ MON_NOM=toto ./bonjour.sh
toto
```

Dans le premier cas le script affiche rien

Après l'exécution je teste si la variable existe encore

```bash
kar : ex02 $ echo $MON_NOM

```

Pour déclaré une variable persistante (d'environnement) :

```bash
kar : ex02 $ export MON_NOM=titi
kar : ex02 $ ./bonjour.sh 
titi
kar : ex02 $ echo $MON_NOM
titi
```

#### Supprimer une variable d'environnement

`env` affiche toutes les variables d'environnement

`unset` les détruits :

```bash
kar : ex02 $ env | grep MON_NOM
MON_NOM=titi

kar : ex02 $ unset MON_NOM

kar : ex02 $ env | grep MON_NOM
kar : ex02 $  # plus rien
```



### Donc avec `make` :

```bash
CFLAGS="-Wall" make ex1
#result :
cc -Wall ex1.c -o ex1
```

### Options

`-g` Generate debug information.

`-o <file>` Write output to file.

`-Wall` Enable "all" (all but the most esoteric) warnings when compiling.



## clean

```bash
make clean
#result :
rm -f ex1
```

Toujours en relation avec ce qu'il y a dans le fichier makefile

## all

```bash
all: clean ex1
```

exécute `clean` et ensuite construit `ex1`

Dans le terminal :

```bash
make all
#result :
rm -f ex1
cc -Wall -g ex1.c -o ex1
```

On peut écrire make tout seul en raccourci :

```bash
make
#result :
rm -f ex1
cc -Wall -g ex1.c -o ex1
```

