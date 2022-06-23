# 05 permissions et propriété

## `sudo`

Root user == super user

`sudo`  vous transforme en super utilisateur

```bash
sudo nano -m titi.txt
# vous pouvez modifier le fichier même si vous n'en avez pas les droits
```

#### Créer un fichier en étant le root user

```bash
sudo touch one-file.txt
ls -l
total 8
-rw-r--r--  1 root  staff   0  4 avr 10:43 one-file.txt
```

On voit que le propriétaire est **root**.

#### Passer au bash root

```bash
sudo bash
whoami
root
exit
exit

whoami
kms
```

`sudo bash` pour utiliser le bash en tant que root

`exit`  pour sortir de ce mode

#### ! la commande ls en mode root, a directement l'option `-a`  activée.



## `chown`

**CH**ange **OWN**ership 

Changer le propriétaire.

```bash
ls -l
-rw-r--r--  1 root  staff   0  4 avr 10:43 one-file.txt
-r--r-----@ 1 kms   staff  24  4 avr 10:40 titi.mm

sudo chown kms one-file.txt 
Password:......

ls -l
-rw-r--r--  1 kms  staff   0  4 avr 10:43 one-file.txt
-r--r-----@ 1 kms  staff  24  4 avr 10:40 titi.mm
```

### Pour plusieurs fichiers

```bash
chown kms *
```



## `chgrp`

**CH**ange **GR**ou**P**e

changer le groupe

```bash
sudo chgrp charlots one-file.txt

ls -l
-rw-rw----@ 1 kms  charlots  10  4 avr 10:56 one-file.txt
```



## Change les propriétaire et les groupes de manière récursive

### `chown -R`   `chgrp -R` 

`-R`  permet de modifier un dossier et son contenu :

```bash
# modifier le propriétaire d'un dossier et de son contenu
sudo chown -R kms closet

ls -lR
drwxr-xr-x  3 kms  staff        96  4 avr 15:05 closet
-rw-rw----@ 1 kms  doudoumasta  10  4 avr 10:56 one-file.txt
-r--r-----@ 1 kms  staff        24  4 avr 10:40 titi.mm

./closet:
drwxr-xr-x  3 kms  staff  96  4 avr 15:05 one

./closet/one:
drwxr-xr-x  3 kms  staff  96  4 avr 15:06 two

./closet/one/two:
-rw-r--r--  1 kms  staff  0  4 avr 15:06 glass.txt

# modifier cette fois le groupe
sudo chgrp -R charlots closet

ls -lR
drwxr-xr-x  3 kms  charlots     96  4 avr 15:05 closet
-rw-rw----@ 1 kms  doudoumasta  10  4 avr 10:56 one-file.txt
-r--r-----@ 1 kms  staff        24  4 avr 10:40 titi.mm

./closet:
drwxr-xr-x  3 kms  charlots  96  4 avr 15:05 one
# ...
```

## `chmod`

**CH**ange **MOD**e

### `chmod [u|g|o]=[rwx] file-name.ext`

u : user, owner

g : group

o : other les autres

w, r, x write read execute

```bash
# retirer les droit de user
chmod u= mon-fichier.txt

# donner tous les droits au groupe
chmod g=rwx mon-fichier.txt

```

### `chmod [+|-][wrx] file.name.ext` 

```bash
# rendre un fichier éxecutable
chmod +x mon-fichier.sh

# enlever le droit en écriture
chmod -w mon-texte.txt
```

