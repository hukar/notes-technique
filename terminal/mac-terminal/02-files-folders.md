# 02 fichiers et répertoires

Les *Users* sont organisé en *Groups*.

Chaque *files* ou *folder* est assigné à un *group*.

**R** readable

**w** writable

**X** executable



## Profiles

**Everyone** juste quelqu'un qui n'est pas le propriétaire

### ! un fichier ou un dossier dans votre dossier de travail peut appartenir à quelqu'un d'autre.

## Commande `ls`

```bash
$ls
Applications	Downloads			Movies		Projects
Desktop				Google Drive	Music			Public
Documents			Library				Pictures
```

Donne le noms des dossiers et fichiers présent dans le répertoire courant.

#### `Ls -l`

Affiche les détails :

```bash
$ls -l

total 0
drwx------@  3 kms  staff    96 20 mar 09:25 Applications
drwx------+  4 kms  staff   128 25 mar 11:34 Desktop
drwx------+  3 kms  staff    96 19 mar 13:43 Documents
drwx------+  4 kms  staff   128 25 mar 11:33 Downloads
drwx------@  8 kms  staff   256  1 avr 09:41 Google Drive
drwx------@ 66 kms  staff  2112 29 mar 14:19 Library
drwx------+  3 kms  staff    96 19 mar 13:43 Movies
drwx------+  3 kms  staff    96 19 mar 13:43 Music
drwx------+  3 kms  staff    96 19 mar 13:43 Pictures
drwxr-xr-x   3 kms  staff    96 20 mar 17:40 Projects
drwx------+  4 kms  staff   128 19 mar 13:43 Public
-rw-r--r--   1 kms  staff     0  1 avr 10:41 titi.txt
```

total : total du poids des fichiers contenus

1. `d` directory / `-` File

2. `rwx` droit en lecture écriture et exécution du propriétaire
   `- - -` idem pour le groupe
   `- - -` idem pour **everyone**

3. `@` ou `+`  ? à voire avec l'héritage de permissions

4. Le nombre de lien

5. `kms`  le propriétaire

6. `staff`  le groupe

7. le poids

8. la date de dernière modification

9. le `path-name`

### `ls -lh`

`h` pour *human readable* 

```bash
$ ls -lh
drwx------@  3 kms  staff    96B 20 mar 09:25 Applications
```

### `ls -F`

`F`  pour fileaffiche un slash pour les dossier, rien pour les fichiers et une étoile pour les exécutables.

```bash
$ ls -F
code.txt	conseil-etat/	planning.txt	script.sh*
```

#### ! on peut combiner les options

```bash
$ ls -laFh
total 0
drwx------+  7 kms  staff   224B  1 avr 14:20 ./
drwxr-xr-x+ 24 kms  staff   768B  1 avr 13:53 ../
-rw-------   1 kms  staff     0B 19 mar 13:43 .localized
-rw-r--r--   1 kms  staff     0B  1 avr 14:16 code.txt
drwxr-xr-x   2 kms  staff    64B  1 avr 14:16 conseil-etat/
-rw-r--r--   1 kms  staff     0B  1 avr 14:16 planning.txt
-rwxrwxrwx   1 kms  staff     0B  1 avr 14:20 script.sh*
```

## Lister ce qu'il y a dans un dossier/fichier spécifique

`ls -la path-name`

```bash
$ ls -la conseil-etat
total 0
drwxr-xr-x  2 kms  staff   64  1 avr 14:16 .
drwx------+ 8 kms  staff  256  1 avr 14:29 ..
```

```bash
$ ls -lF script.sh
-rwxrwxrwx  1 kms  staff  0  1 avr 14:20 script.sh*
```

## Avoir des infos sur un fichier `file`

```bash
$ file season21_ep04_ss01.jpg 
season21_ep04_ss01.jpg: JPEG image data, Exif standard: [TIFF image data, little-endian, direntries=0], baseline, precision 8, 578x327, frames 3
```



## `ls -S`

Trier par 'size' du plus grand au plus petit

## `ls -r`

Inverser l'ordre d'affichage

## `ls -R`

Affiche de manière récursive le contenue.

## `man`

`/text` pour trouver les occurences du texte dans le manuel (`man`)

`n` Next = prochaine occurence

`shift` + `n` occurence précédente

