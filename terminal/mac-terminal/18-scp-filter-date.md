# 18 Utiliser scp en filtrant par date

## a- Filtrer des fichiers par date

### `find` 

### `-mtime` 

**Modification time** la dernière fois que le contenu du fichier a été modifié

### `-atime`

**Access time** la dernière fois que le fichier a été lu


### `-ctime`

**Change time** la dernière fois que le fichier ou ses meta-données ont été modifié

### `-mtime 2`

le fichier a été modifié il y a deux jours entre les dernière 48 et 72 heures

### `-mtime -2`

Le fichier a été modifié les dernières 48 heures

### `-mtime +2`

Le fichier a été modifié il y a plus de 2 jours (au moins 72 heures)

```bash
ls -l `find . -type f -mtime -4`
-rw-r--r--@ 1 kms  staff      6148 Sep 19 10:47 ./.DS_Store
-rw-r--r--  1 kms  staff  12865192 Sep 17 01:59 ./access_log.1568592000
-rw-r--r--  1 kms  staff  10880972 Sep 19 01:59 ./access_log.1568764800
-rw-r--r--  1 kms  staff   2935774 Sep 19 10:45 ./access_log.1568851200
-rw-r--r--@ 1 kms  staff    124701 Sep 17 01:56 ./error_log.1568592000
-rw-r--r--  1 kms  staff    122132 Sep 19 01:56 ./error_log.1568764800
-rw-r--r--  1 kms  staff     45231 Sep 19 10:42 ./error_log.1568851200
```

Utilisation de ls -l pour voire les dates (depuis le jour du 30 septembre) -> 4 jours avant

### ! je n'ai pas les fichiers du 16 !!?

## Utilisation de -newerXY

`-newermm` est équivalent de `-newer` et attend un nom de fichier.

Pour utiliser une date on utilise `-newermt` Modified time (ou `-newerat` Accessed time ou `-newerct` Changed time ou `-newerBt` Inode creation time)

### On veut comparer le fichier courant avec une date formatée on utilise donc : `-newermt`

```bash
ls -l `find . -type f -newermt 2019-09-14 ! -newermt 2019-09-30`

-rw-r--r--@ 1 kms  staff      6148 Sep 19 10:47 ./.DS_Store
-rw-r--r--  1 kms  staff   8576955 Sep 14 01:59 ./access_log.1568332800
-rw-r--r--  1 kms  staff    189669 Sep 15 01:59 ./access_log.1568419200
-rw-r--r--  1 kms  staff   1524743 Sep 16 01:59 ./access_log.1568505600
-rw-r--r--  1 kms  staff  12865192 Sep 17 01:59 ./access_log.1568592000
-rw-r--r--  1 kms  staff  10880972 Sep 19 01:59 ./access_log.1568764800
-rw-r--r--  1 kms  staff   2935774 Sep 19 10:45 ./access_log.1568851200
-rw-r--r--  1 kms  staff    112406 Sep 14 01:56 ./error_log.1568332800
-rw-r--r--  1 kms  staff    101376 Sep 15 01:56 ./error_log.1568419200
-rw-r--r--  1 kms  staff    101852 Sep 16 01:56 ./error_log.1568505600
-rw-r--r--@ 1 kms  staff    124701 Sep 17 01:56 ./error_log.1568592000
-rw-r--r--  1 kms  staff    122132 Sep 19 01:56 ./error_log.1568764800
-rw-r--r--  1 kms  staff     45231 Sep 19 10:42 ./error_log.1568851200
```

On voit que le 14 est inclus.



## Utilisation avec `scp`

```bash
ssh info_service@172.16.10.131 find /opt/apache/logs -type f -newermt 2019-09-12 ! -newermt 2019-09-30
```

Voici une commande me donnant la liste souhaitée :

```bash
/opt/apache/logs/access_log.1568505600
/opt/apache/logs/error_log.1568851200
/opt/apache/logs/access_log.1568419200
/opt/apache/logs/error_log.1568160000
/opt/apache/logs/access_log.1568937600
/opt/apache/logs/access_log.1568678400
/opt/apache/logs/error_log.1568764800
...
```

Avec une boucle `for in` cela fonctionne mais me demande le mot de passe à chaque itération !

```bash
for i in `ssh info_service@172.16.10.131 find /opt/apache/logs -type f -newermt 2019-09-12 ! -newermt 2019-09-30`
> do
> scp info_service@172.16.10.131:$i ./Desktop/test
> done
```

à améliorer ...

