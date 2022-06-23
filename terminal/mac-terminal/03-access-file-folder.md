# 03 Accéder à ses fichiers et ses répertoires



## Change Directory `cd`

Taper `cd` tout seul ramène à son répertoire personnel. (Home)

## Créer, écrire dedans et afficher un fichier

`touch toto.txt`  crée un fichier.

`echo "coco" >> toto.txt`  écrit à la suite dans le fichier.

`cat toto.txt`  affiche le fichier.

### copier un fichier dans un nouveau fichier

```bash
cat titi.txt >> toto.txt
```

`>>`  copie le contenu de titi.txt à la suite dans toto.txt, si toto.txt n'existe pas elle le crée (la commande `>>`).

## `cat`

Affiche le contenu d'un dossier

Viens de l'anglais catenate = concaténer

`-n` affiche le numéro de ligne

```bash
$ cat -n toto.txt

1	coucoun titi
2	mon p'tit loup
3	coucoun titi
4	mon p'tit loup
```

## `more`

affiche le contenu d'un dossier par le début. (page par page)

## `less`

affiche le contenue dans un nouvel *'espace'*.

`q` pour sortir

### recherche de chaîne avec `/`

```bash
/to
```

`n`  pour avoir **N**ext occurence

`shift` + `n`  pour avoir Previous occurence

`g`  pour aller au début

`shift` + `g`  pour aller à la fin 

## `open`

Ouvre un fichier ou un dossier

`-a`  Dans une application

```bash
# un fichier
open -a Typora titi.md

# un dossier
open -a Typora mon-dossier/
```

`.`  Représente le répertoire courant

```bash
open .
# ouvre le répertoire courant dans le finder
```

#### ! `open`  devient `xdc-open`  sur Ubuntu (ou autre linux)

