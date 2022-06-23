# 13 Les fonctions

```
function nom_de_la_fonction() {
  cmd1
  cmd2
  cmd3
  ...
}

# ou bien sans le mot clé function

nom_de_la_fonction() {
  cmd1
  cmd2
  cmd3
  ...
}
```

Exemple :

```bash
#! /bin/bash

function internet() {
    ping -c 4 8.8.8.8 # serveur google

    if [ $? -eq 0 ] # code retour = 0 tout va bien
    then
        echo "la connexion fonctionne"
    else
        echo "pas de connection"
    fi
}

internet
```

Rappel : `$?`  code retour de la précédente commande



## Paramètres de fonction

#### ! `$0`fait toujours référence au nom du script et pas au nom de la fonction

```bash
#! /bin/bash

function internet() {
    ping -c $1 $2 # paramètres de la fonction

    if [ $? -eq 0 ]
    then
        echo "la connexion fonctionne"
    else
        echo "pas de connection"
    fi
}

internet $1 $2  # paramètres du script
```

On fait passer les paramètres du script aux paramètres de fonction.

## Scope des variables

Une variable initialisée dans une fonction ne peut être appelée qu'après que la fonction est été éxécutée.

Elle devient une variable globale.

#### ! utiliser plutôt des variables locales cf plus loin

```bash
#! /bin/bash

function init_variable() {
    echo "initialisation de la variable name"

    name="michel"
}

echo $name # n'affiche rien car name set initialisé dans la fonction
```

```bash
#! /bin/bash

function init_variable() {
    echo "initialisation de la variable name"

    name="michel"
}

init_variable
echo $name
```

```
initialisation de la variable name
michel
```

## Variable locale : `local`

Variable utilisable seulement dans la fonction.

```bash
#! /bin/bash

function init_variable() {
    echo "initialisation de la variable name"

    local name="michel"

    echo "utlisation local : $name"
}

init_variable
echo "utilisation en dehors de la fonction :$name" # n'affiche pas name
```

```
initialisation de la variable name
utlisation local : michel
utiolisation en dehors de la fonction : 
```

## Code de retour de fonction `return 0`

Un dé avec `return`

```bash
#! /bin/bash

# thimble = dé
function thimble() {
    nb=$(( (RANDOM % $1) + 1 ))

    return $nb
}

thimble $1

result=$? # on récupère le code de la dernière commande

echo $result
```

## Teste de l'éxistence de fichiers

```bash
#! /bin/bash
function nb_argument() {
    if [ $1 -eq 0 ]
    then
        echo "Vous devez introduire des noms de fichier"
        exit 2
    fi
}

function fichier_exist() {
    [ -f $1 ] && echo "le fichier $1 existe" || echo "le fichier $1 n'existe pas"
}

nb_argument $#  # on passe le nombre d'argument du script en argument de la fonction

for f in $* # $* set la liste des paramètres
do
    fichier_exist $f
done
```

Variante de la fonction `fichier_exist`

```bash
function fichier_exist() {
    ls $1 2> err.txt # 2> on redirige l'erreur vers un fichier

    if [ $? -eq 0 ]
    then
        echo "le fichier $1 existe"
    else
        echo "le fichier $1 n'existe pas"
    fi
}
```

