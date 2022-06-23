# 07 `find`

## `find .`

trouver tout -> arrêter le processus `ctrl` + `c`

```bash
mkdir -p chest/box/jewelry

touch chest/box/jewerly/a.txt
touch chest/box/jewerly/aa.txt
touch chest/box/jaa.txt

# chercher dans chest les fichiers et les dossiers commençant par b
find chest -name "b*"

# chercher les fichiers et les dossiers contenant un a
# attention il faut écrire "*a*" (avec des guillemets)  
# car s'il y a plus d'un élément une erreur est lancée
find chest -name "*a*"
```

## `find repertoire -type d`

Retrouve les dossier uniquement

```bash
find chest -type d

chest
chest/box
chest/box/jewelry
```

## `find repetoire -type f`

Les fichiers uniquement

```bash
find chest -type f

chest/box/jaa.txt
chest/box/jewelry/aa.txt
chest/box/jewelry/a.txt
```

## `find . -type d -name "*e*"`

On cumule les options

```bash
find . -type d -name "*o*"

./chest/box
./closet
./closet/one
./closet/one/two
```

#### ! ne pas oublier les guillemets

### Limiter la profondeur de recherche `-maxdepth 1`

```bash
find . -maxdepth 1 -type f

./.DS_Store
./.localized
./myscript.sh
./ligne.txt
./text.txt
```



## grep

Rechercher des lignes à l'intérieur d'un fichier.

```bash
grep mi closet/something.txt

milou
mickey
```

`grep -i`  insensible à la casse

```bash
grep -i mi closet/something.txt

milou
mickey
Milou
```

### Avec pipe `|`

```bash
ls -a | grep e

.DS_Store
.localized
chest
closet
```

`grep -v`  ne contient pas

```bash
ls -a | grep -v o

.
..
chest
text.txt
```

Renvoie les lignes ne contenant pas **o**.