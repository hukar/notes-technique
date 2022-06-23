# Le `case`

```
case "$variable" in
	case_one)
		cmd1
		cmd2
		;;
	case_two)
		cmd1
		cmd2
		;;
	case_three)
		cmd1
		cmd2
		;;
esac
```

exemple :

```bash
#! /bin/bash

case "$1" in
    start)
        echo "le serveur est lancé"
        ;;  # break
    stop)
        echo "le serveur est arrêté"
        ;;
    *)
        echo "hey caramba!"
        ;;
esac
```

### Retrouver un processus

```bash
ps -a | grep apache2
```

```
4627 ttys002    0:00.00 grep apache2
```

## Le OU ` | `

```bash
case "$1" in 
    lundi | LUNDI)
        echo "lundi"
        ;;
    mardi | MARDI)
        echo "mardi"
        ;;
    mercredi | MERCREDI)
        echo "mercredi"
        ;;
    jeudi | JEUDI)
        echo "jeudi"
        ;;
    *)
        echo "dimanche"
        ;;
esac
```

## Utilisation des classes de caractères

```bash
case "$1" in 
    [lL][uU][nN][dD][iI])
        echo "lundi"
        ;;
    [mM][aA][rR][dD][iI])
        echo "mardi"
        ;;
    *)
        echo "dimanche"
        ;;
esac
```

```
./case3.sh MarDi
mardi

./case3.sh lUnDi
lundi
```

## Exercice sur `case`

```bash
#! /bin/bash

fichier=$1

cmd=$2

if [ -z $fichier ] || [ -z $cmd ]; then
    echo "nombre d'argument incorrecte"
    exit 1
fi

if [ -f $fichier ]
then
    case "$cmd" in
        copy)
            if [ ! -e tmp/copie ]
            then
                echo "Création du dossier /tmp/copie"
                mkdir -p tmp/copie
            fi
            echo "copie du fichier $fichier dans tmp/copie"
            cp $fichier tmp/copie
            ;;
        delete)
            read -p "êtes-vous sûre ? (y/n) " responseThe 

            case "$response" in
                y)
                    rm $fichier
                    echo "fichier $fichier supprimé"
                    ;;
                n)
                    echo "fichier $fichier non supprimé"
                    ;;
                *)
                    echo "mauvaise instruction"
                    ;;
            esac
            ;;
        read)
            cat $fichier
            ;;
        *)
            echo "commande non disponible"
            ;;
    esac
else
    echo "$fichier n'est pas un fichier valide"
fi

echo "fin du programme"
```

### OU avec `if`

```bash
if [ -z $fichier ] || [ -z $cmd ]; then
```

### not exist

```bash
if [ ! -e tmp/copie ]
```

