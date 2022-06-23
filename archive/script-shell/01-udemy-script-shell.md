# 01 Udemy Script Shell

Un script doit être exécutable :

```bash
chmod a+x script.sh
```

`a` *all* : tous les utilisateurs

`+x` *exec* : rend le fichier exécutable

### Pour éxecuter

Le chemin absolu :

```bash
/home/mimine/script.sh
```

Le chemin relatif si je m'y trouve :

```bash
./script.sh
```

## Le Shebang

Première ligne d'un script

`#!` + `chemin interpréteur`

```bash
#!bin/bash
```

C'est le chemin vers l'interpréteur de script.

Il en existe d'autre :

```bash
#!bin/sh
#!bin/csh
#!bin/zsh
```

### ! S'il n'y a pas de Shebang, utilisation du shell du terminal utilisé.

On peut exécuter un script python

`pyton.sh` :

```python
#! /usr/bin/python
print('hello coco')
```

```bash
chmod a+x python.sh
./python.sh
hello coco
```

### ! par convention les variables sont en majuscule.

```bash
#! /bin/bash

MA_VARIABLE="toto";

echo $MA_VARIABLE;
```

Pas d'espace  autour du signe égal.

`$`  pour afficher le contenu de la variable.

`#` pour écrire un commentaire.

### Écriture avec `${...}`

Dans certaine situation on peut utiliser l'écriture `${...}`

```bash
MA_VARIABLE="toto";
NOM="chacha";
PRENOM="chichi";

echo $MA_VARIABLE;
# comme on colle la variable NOM à la virgule on ne peut pas écrire $NOM, car l'interpréteur cherchera alors une varible NOM,
echo "je suis $PRENOM ${NOM}, et mon copain est $MA_VARIABLE";
```