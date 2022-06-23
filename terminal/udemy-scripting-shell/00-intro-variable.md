# 00 Intro et Variable

## Ouvrir un fichier ou un dossier dans une application

```sh
open -a "nom de l'application" mon-dossier.ext
# exemple :
open -a "typora" script-shell
```

## Voire où se trouve une commande

```sh
type -a python
```

## Ajouter à tout le monde le droit d'éxécuter un script

```bash
chmod a+x monscript.sh
```

## Shebang `#!/bin/bash` 

Le **shebang** indique au système d'exploitation que le fichier est un script et pas un fichier binaire.

`# = sharp` => SH

`! = bang` => BANG

=> SH'BANG => SHEBANG

```bash
ls -la /bin | grep bash
```

```
-r-xr-xr-x   1 root  wheel   618416 21 mar 06:14 bash
```

Si le `shebang`  n'est pas mentionné, le script utilisera le shell qui le lance comme interpréteur (si c'est un bash ça va)

Le `shebang`  spécifie l'interpréteur à utiliser : `#! /usr/bin/python`  par exemple

```bash
which python
/usr/bin/python
type python
python is /usr/bin/python
```

`type`  ou `which` peuvent aider à trouver l'adresse d'un interpréteur.

## Variables

Par convention les variables sont en majuscule. Il n'y a pas d'espace autour du égal.

```bash
MA_VARIABLE="salut"
```

#### ! les noms de variable ne contiennent que des majuscules, minuscules, underscore et des chiffres mais pas en premiere place.

### Inclure une variable dans un mot `${MA_VAR}`

Si l avariable touche un caractère, on utilise la notation avec accolades.

```bash
#! /bin/bash
PRENOM="michel"
NOM="Roublard"

echo "salut $PRENOM, commant va la famille $NOM" # ici pas de problème avec la virgule et le guillemet

AGE="45"
echo "tu as bien ${AGE}ans" # par contre avec le mot ans on est obligé de mettre des accolades
```

### Syntaxe alternative : enregistrer le résultat d'une commande dans une variable var=\`cmd\`

```bash
machine1=`hostname`
machine2=$(hostname)
echo $machine1 $machine2
MBP-de-karim.raadvst-consetat.be MBP-de-karim.raadvst-consetat.be
```

