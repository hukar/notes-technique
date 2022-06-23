# 05 Paramètres de position

Il y en a dix `$0` à `$9`

## Lister les paramètres

```bash
#! /bin/bash
echo "position des paramètres"
for i in {0..9} 
do
     output="\$$i : "
     eval output+=\$$i
     echo $output
done
```

```
$0 : ./position.sh
$1 : titi
$2 : toto
$3 :
$4 :
...
```



`eval`  prend une chaîne en tant qu'argument et l'évalue comme si elle était saisie sur une ligne de commande.

### Autre syntaxe `${!i}`

```bash
for i in {0..9} 
do
     echo ${!i}
done
```



## `$@`  ensemble des paramètres (aussi `$*`)

Une boucle infinie

```bash
#! /bin/bash
echo "infinity"
$0 $@
```

```
infinity
infinity
infinity
...
```

## `$#` le nombre de paramètre

```bash
echo "le nombre de paramètres \$# : $#"
echo "la liste des paramètres \$* : $*"
```

```
./parametre-position.sh titi toto tata
le nombre de paramètres $# : 3
la liste des paramètres $* : titi toto tata
```



## `$*` La liste des paramètres 

```bash
for i in $*
do
    echo $i
done
```

```
./test-param.sh titi toto tata
titi
toto
tata
```





## `shift`

Décale les paramètres de un (pour en avoir plus que 9).

```bash
echo "premier argument : $1"
shift # on décale de une place
echo "deuxième argument : $1"
```

```
premier argument : titi
deuxième argument : toto
```

## Vérifier l'existence d'un argument `[ -z ]`

```bash
echo "l'argument1 a pour valeur $1"

if [ -z $1 ]
then
    echo "Vous n'avez pas entré d'argument"
    echo "pour éxécuter le script"
fi
```



## Différence entre `$*`  et `$@`

```bash
for i in $@
do
    echo "@ $i"
done

for i in $*
do
    echo "* $i"
done

# on éxécute ./parameters-advanced.sh foo bar baz 'long arg'
```

```
@ foo
@ bar
@ baz
@ long
@ arg
* foo
* bar
* baz
* long
* arg
```

Chaque valeur est égrainée, même comportement.

```bash
for i in "$@"
do
    echo "@ $i"
done

for i in "$*"
do
    echo "* $i"
done

# on éxécute ./parameters-advanced.sh foo bar baz 'long arg'
```

```
@ foo
@ bar
@ baz
@ long arg
* foo bar baz long arg
```

On peut dire que `"$@"`  est le comportement attendu.

#### Nombre d'arguments des différentes syntaxes

```bash
main() {
    echo "Main sees" $# " args"
}

main $*
main $@
main "$*"
main "$@"
```

```bash
./parameters-advanced.sh 'a b c' 'd e' f g
```

```bash
Main sees 7  args # $*
Main sees 7  args # $@
Main sees 1  args # "$*"
Main sees 4  args # "$@" c'est la valeur souhaitée
```

#### ! Il faut utiliser la syntaxe "$@" pour itérer sur les arguments

| Syntaxe | Resultat effectif                  |      |
| ------- | ---------------------------------- | ---- |
| $*      | `$1`  `$2`  `$3`  … `${n}`         |      |
| $@      | `$1`  `$2`  `$3`  … `${n}`         |      |
| "$*"    | `"$1c$2c$3c...c${n}"`              |      |
| "$@"    | `"$1"`  `"$2"`  `"$3"`  … `"${n}"` |      |

Où **c** est le premier caractère de la variable d'environnement `$IFS` (Internal Field Separator)