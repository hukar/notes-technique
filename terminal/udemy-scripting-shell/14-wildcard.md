# 14 Les wildcard

## `*` match avec tout les caractères

```bash
ls exo*
```

```
exo-case.sh             exo-param-position.sh
exo-facile.sh           exo-random.sh
exo-fichier-existe.sh
```

```bash
ls t*.txt
```

```
tata.txt        titi.txt
```

## `?`  match avec un seul caractère (n'importe lequel)

```bash
ls c*.sh # n'importe quel caractère et n'importe quel nombre de caractère(s)
case.sh         case3.sh        compteur.sh
case2.sh        cat.sh          condition.sh

ls c???.sh # trois caractères éxactement
case.sh
```

## `[]`  les classes de caractères

`[agi]`  un caractère et un seul pouvant avoir la valeur `a` ou `g` ou `i`

```bash
ls [ab]*.sh
act.sh
boucl.sh
boucle-infini-parametre.sh
```

## `[!]`  Exclure des caractères

```bash
ls *[!s]? # l'avant dernier caractère ne peut pas être s
eof.txt                 tata.txt
err.txt                 titi.txt
minus.txt               whitout_minus.txt
tata.html
```

Supprimer des fichiers et des dossiers avec confirmation :

```bash
rm -idR *[!s]? # -i confirmation -d dossier -R recursivité
```

## `[a-t]`  Les plages de caractères

```bash
ls [a-f][a-m]* # la première lettre entre a et f et la deuxième entre a et m puis tout ce qu'on veut
act.sh          case2.sh        cat.sh
case.sh         case3.sh        empty-string.sh
```

## Les classes de caractère

```bash
[[:alpha:]] # [a-zA-Z]
[[:alnum:]] # [a-zA-Z0-9]
[[:digit:]] # [0-9]
[[:upper:]] # [A-Z]
[[:lower:]] # [a-z]
[[:space:]] # tout les caractères d'espacement
```

```bash
ls *[[:digit:]]*
case2.sh        case3.sh
```

## `\`  caractère d'échappement

```bash
touch t\?t\? # pour créer un fichier t?t?
```

```bash
ls t?t? # ? est un wildcard
t?t?    tata    titi    toto # affiche quatre fichiers

ls t\?t\? # \? est le littéral (le caractère) ?
t?t? # affiche un seul fichier
```

