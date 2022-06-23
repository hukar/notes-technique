# 09 Utilisation des parenthèses, accolades et crochets

## `[]`  et  `[[]]`  Les tests

L'écriture `[[]]`  permet plus de syntaxe :

```bash
if [[ $word = allo && ! -z $word ]]

# simple crochet
if [ $word = allo -a ! -z $word ]
```

Avec les doubles crochets ont est authorisé à utilisé `&&` et `||`  à la `-a` et `-o`.

## `{ … }`  Expansion

#### Les accolades permettent de délimiter un nom de variable :

```bash
name="titi"
age=99

echo "mon fichier est $name_$age.txt"
echo "mon fichier est ${name}_$age.txt"
```

```
mon fichier est 99.txt
mon fichier est titi_99.txt
```

Dans certain cas on utilise `${ … }`  pour lever une ambiguïté.

#### Les accolades servent aussi à l'expansion des paramètres

Tronquer :

```bash
var="abcde"

echo ${var%c*}
```

```
ab
```

Substituer :

```bash
var="hello les amis, ça va"

echo ${var/amis/cocos}
```

```
hello les cocos, ça va
```

Valeur par défaut :

```bash
salut="salut"

hello="hello"

unset salut

echo ${salut:-$hello}
```

```
hello
```

#### Création de liste de `string`

```bash
echo t{oto,ata,iti}.txt
```

```
toto.txt tata.txt titi.txt
```

Pour renomer un fichier :

```bash
mv error.log{,.OLD}
# ce qui donne après expansion mv error.log error.log.OLD
```

#### Avec `{a..b}` :

```bash
for num in {000..2}
do
    echo $num
done
000
001
002

echo {00..8..2}
00 02 04 06 08

echo {a..m..3}
a d g j m
```

## `{ cmd1; cmd2; }`Exécuter plusieurs commandes : block de code

```bash
{ cd; ls -la; } | grep '^t'
total 120
```

#### ! espace avant et point-virgule après chaque commande



## `(( … ))` Opérations arithmétique

```bash
a=4

((a++))

echo $a
```

```
5
```

Les doubles parenthèses permettent de ne pas ajouter le dollar devant le nom des variables numériques et d'ajouter des espaces pour la lisibilité.

```bash
(( nb = 42 ))
(( nb2 = 5 ))

(( a = nb + nb2 ))

echo $(( a + 1 ))
```

```
48
```

## `( … )`  Créer un sous-shell

```bash
kms: Desktop $ pwd
/Users/kms/Desktop
kms: Desktop $ (cd ;pwd)
/Users/kms # ici la valeur de pwd dans le sous-shell
kms: Desktop $ pwd
```

