# 02 shell scripting EOF cat

command `cat`

concatenate :

Juste `cat` pour afficher dans le terminal

```bash
$ cat
hello
hello
coco
coco
# ctrl + d pour sortir
```

`cat mon_fichier.txt` pour afficher le contenu d'un fichier :

```bash
$ cat toto.txt
toto
toto
```

`cat f1.txt f2.txt` pour afficher le contenu de deux fichiers :

```bash
$ cat titi.txt toto.txt
titi
toto
toto
```

options :

`-n` affiche les numéro de ligne

```bash
$ cat -n toto.txt
1  toto
2
3  toto
4  toto
```

## Écrire dans un fichier (et le créer)

```bash
# syntaxe :
cat << DELIMITER > mon_fichier.truc
cmd -o schoml
other_cmd -p bluck
DELIMITER
```

exemple :

```bash
$ cat << NINI > nini.result
> echo \$PWD
> echo $PWD
> NINI
```

`nini.result` :

```bash
echo $PWD
echo /c/Users/kms/Documents/programmation/script-shell
```

`EOF` est souvent pris pour délimiteur.

```bash
cat << EOF > eof.txt
> une première phrase
> une deuxième phrase
> EOF
```

## `<<`  Passe entrées de la console à une commande

Syntaxe :

```bash
cmd << DELIMITER
> ...
> ...
> ...
DELIMITER
```

Exemple :

```bash
grep a << EOF
> un petit chat
> est sur le mur
> il boit du lait
> EOF
un petit chat # les deux lignes trouvées
il boit du lait
```

On peut les enregistrer dans un fichier :

```bash
grep o << EOF > resultat.txt # > on écrit dans un fichier
> un popo
> deux titi
> trois caca
> EOF

cat resultat.txt 
un popo
trois caca
```



`cat << DELIMITER`  permet de lire plusieurs entrée jusqu'au délimiteur.

`> mon fichier.txt`  écrit dans le fichier

### exemple avec `cat << DELIMITER`

```bash
sql=$(cat << DELIMITER
> SELECT foo
> FROM db
> WHERE foo='baz'
> DELIMITER
> )
```

Les différentes lignes ont été mémorisée dans la variable :

```bash
echo "$sql" # les guillemets sont importantes pour  garder les saut de ligne
SELECT foo
FROM db
WHERE foo='baz'
```

## Exemple de script avec `<<`

```bash
#! /bin/bash

read -p "entrez une voyelle " voy

grep $voy << EOF > eof.txt
> ici une ligne
> une seconde ligne
> une troisième ligne avec a
> EOF
```

## Enlever les `tab`  avec `<<-`

Dans le script :

```bash
cat <<- DELIMITER > minus.txt
une phrase
	deux phrase
trois phrase
		quatre phrase
DELIMITER
```

Dans `minus.txt`  les `tab`sont supprimés :

```
une phrase
deux phrase
trois phrase
quatre phrase
```

