# Monter un disque réseau

## `mount`

Dans un réseau windows les diques réseau sont disponnible grâce au protocole samba.

#### `mount -t smbfs //MACHINENAME/SHARENAME ./MYDIRECTORY` 

```bash
mkdir w
mount -t smbfs //raadvst-consetat.be/shared/infcel ./w
Password for raadvst-consetat.be: 

```

Syntaxe alternative :

`mount_smbfs //servername/sharename ./mydirectory`



Pour dé-monter :

#### `umount ./MYDIRECTORY`

```bash
umount ./w
```



## `open`

On peut aussi utiliser `open` qui va monter le disque dans le répertoire `/Volumes`

#### `open 'smb://machinename/sharename'`

```bash
open 'smb://raadvst-consetat.be/shared/infcel'

kms: Desktop $ ls /Volumes/
Macintosh HD	infcel
```

Sur Os X les disques sont montés dans le répertoire `/Volumes` 

## Automatiser le montage du disque réseau

Utilisation de l' `automount` avec un disque réseau sous **Windows**

Lien vers l'article d'origine : https://useyourloaf.com/blog/using-the-mac-os-x-automounter/

### 1. créer un répertoire

Lorsque je crée un répertoire dans `/Volumes`, OsX l'éfface automatiquement.

Je vais donc créer un répertoire à la racine :

```bash
sudo mkdir /mnt/Resources # => mount
```

### 2. modifier `auto_master`

`automount` utilise le fichier `/etc/auto_master`.

Ce fichier décrit les dossiers gérés par `automount`.

Nous allons placer nos map dans un fichier séparé : `auto_resources` et créer un lien dans `auto_master` .

```bash
#
# Automounter master map
#
/Users/kms/mnt/Resources     auto_resources # <- ici notre entrée
+auto_master		# Use directory service
/net			-hosts		-nobrowse,hidefromfinder,nosuid
/home			auto_home	-nobrowse,hidefromfinder
/Network/Servers	-fstab
/-			-static
```

J'ai déplacé le dossier `mnt` de la racine à mon `home` car depuis la mise à jour vers **Catalina**, une partie du disque système est en Read-Only par sécurité.

La mise à jour avait d'ailleurs écrasé mon fichier `auto_master`.

### 3. Créer le fichier `auto_resources`

Tout d'abord le créer physiquement :

```bash
sudo touch /etc/auto_resources
```

Nous allons ajouter un `map` à ce fichier :

Si on veut /mnt/Resources/w

`auto_ressources`

```bash
w -fstype=smbfs ://raadvst-consetat.be/shared/infcel/
```

Il faut ajouter le login et mot de passe :

#### `://login:password@servername/pathname`

```bash
w -fstype=smbfs ://kms:Avenger4Ever@raadvst-consetat.be/shared/infcel/
```

`-fstype=smbfs`  type du file system = Samba (emule le file system de Windows sur Unix)

### 4. Lancer `automount`

Maintenant on informe `automount ` que tout est ok :

```bash
automount -vc
```

### 5. Modifier le temps de démontage `unmount`

Les configuration de `automount` sont dans le fichier `/etc/autofs.conf` :

```bash
#
# This file is used to configure the automounter
#

# The number of seconds after which an automounted file system will
# be unmounted if it hasn't been referred to within that period of
# time.  The default is 10 minutes (600 seconds).
# This is equivalent to the -t option in automount(8).
AUTOMOUNT_TIMEOUT=3600

# ...
```



