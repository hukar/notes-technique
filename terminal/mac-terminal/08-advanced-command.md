# 08 Les commandes avancées

## `.profile`

### Ajouter des couleurs à la commande `ls`

```bash
nano -m .profile 
> export CLICOLOR="YES" # dans le fichier .profile

source .profile # recharge .profile
```

Maintenant les dossier, fichiers, exécutables ont des couleurs différentes avec la commande `ls`.

`source`  permet de rendre `.profile` efficiente.

### Créer des alias

```bash
nano -m .profile
> alias lf="ls -Fhla"

source .profile
lf

drwxr-xr-x+ 32 kms   staff   1,0K  5 avr 10:43 ./
drwxr-xr-x   7 root  admin   224B  1 avr 10:34 ../
-rw-r--r--   1 kms   staff   266B  1 avr 15:27 .489429.padl
-r--------   1 kms   staff     8B 19 mar 13:43 .CFUserTextEncoding
-rw-r--r--@  1 kms   staff    10K  3 avr 14:55 .DS_Store
drwxr-xr-x   3 kms   staff    96B 20 mar 17:39 .IdentityService/
# ...
```

## Les variables

```bash
myvar=234
echo $myvar
234

echo "myvar equal to $myvar"
myvar equal to 234

v=variable
echo " $v "
 variable
 
vv="variable variable"
echo " $vv "
 variable variable
```

### `unset`

Désassigne une variable

```bash
unset vv
echo " $vv "
  
```

### variable contenant une commande

```bash
myls="ls -al"
echo $myls
ls -al

# lancer la commande
$myls
drwx------+  8 kms  staff      256  5 avr 10:31 .
drwxr-xr-x+ 32 kms  staff     1024  5 avr 10:43 ..
-rw-r--r--@  1 kms  staff     6148  5 avr 10:31 .DS_Store
-rw-r--r--   1 kms  staff        0 19 mar 13:43 .localized
drwxr-xr-x   3 kms  staff       96  4 avr 16:42 chest
drwxr-xr-x   5 kms  charlots   160  5 avr 10:14 closet
-rw-r--r--   1 kms  staff       73  5 avr 10:31 ligne.txt
-rw-r--r--   1 kms  staff       48  5 avr 10:28 text.txt
```

### Variables d'environnement 

```bash
echo $USER
kms

echo $HOME
/Users/kms

echo $PATH
/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/share/dotnet:~/.dotnet/tools:/Library/Frameworks/Mono.framework/Versions/Current/Commands
```

Le `PATH`  rassemble tous les chemins où le système va chercher les commandes tapées dans le terminal.

## `read`

Lire une valeur dans le terminal et la stocker dans une variable.

```bash
read toto
> toto

echo $toto
toto
```

`read -p`  ajoute un texte

```bash
read -p "hello what is your age" age
> hello what is your age 43

echo $age
43
```

`read -s`  **S**ecret cache ce qu'on tape

```bash
read -sp "please your password : " pwd
```

## executer un fichier

```bash
nano -m
> echo "hello"
> echo "titi"

chmod +x myscript.sh 
myscript.sh
-bash: myscript.sh: command not found # il faut un chemin pas juste un nom de fichier

./myscript.sh 
hello
titi
```

## `which`

**B**ourne **A**gain **SH**ell

Permet de localiser une commande.

```bash
which ls
/bin/ls

which bash
/bin/bash

which clear
/usr/bin/clear
```

Ce sont des programmes que l'on peut très bien exécuter avec juste leur chemin

```bash
/bin/ls

Applications              etc
Library                   home
Network                   installer.failurerequests
System                    net
Users                     private
Volumes                   sbin
bin                       tmp
cores                     usr
dev                       var
```

##  `$(command)`

Passer le résultat d'une commande comme contenu d'une variable.

```bash
search=$(find . -type d -name "*o*")

echo $search
./chest/box ./closet ./closet/one ./closet/one/two
```

### Utile pour sauvegarder un chemin

```bash
mkdir -p nan/coco/titi/toto
cd nan/coco/titi/toto
pwd
/Users/kms/Desktop/nan/coco/titi/toto

toto=$(pwd) # le résultat de pwd
echo $toto
/Users/kms/Desktop/nan/coco/titi/toto

cd # retour au home directory

touch $toto/mini.txt # accès facile au répertoire toto
ls $toto
mini.txt

```

## Lister tous les fichiers ou dossiers commençant par une lettre choisie

```bash
ls -l P*
```

Avec une certaine lettre dans leur nom

```bash
ls . | grep c

Applications
Documents
Music
Pictures
Projects
Public
```

### Trier une liste par ordre de taille `ls -S`

```bash
ls -lhS

drwx------@ 66 kms  staff   2,1K 29 mar 14:19 Library
drwx------+ 10 kms  staff   320B  5 avr 14:24 Desktop
-rw-r--r--   1 kms  staff   285B  4 avr 16:28 files.txt
-rw-r--r--   1 kms  staff   285B  4 avr 16:33 neo.txt
drwx------@  8 kms  staff   256B  5 avr 09:51 Google Drive
drwx------+  6 kms  staff   192B  3 avr 16:18 Documents
drwx------+  4 kms  staff   128B  3 avr 11:46 Downloads
drwx------+  4 kms  staff   128B 19 mar 13:43 Public
drwx------@  3 kms  staff    96B 20 mar 09:25 Applications
drwx------+  3 kms  staff    96B 19 mar 13:43 Movies
drwx------+  3 kms  staff    96B 19 mar 13:43 Music
drwx------+  3 kms  staff    96B 19 mar 13:43 Pictures
drwxr-xr-x   3 kms  staff    96B 20 mar 17:40 Projects
-rwxr-xr-x   1 kms  staff     0B  5 avr 10:43 titi.sh
```

