# 20 Changer de shell

## La liste de tous les shells du système

```bash
kar : ~ $ cat /etc/shells
# List of acceptable shells for chpass(1).
# Ftpd will not allow users to connect who are not using
# one of these shells.

/bin/bash
/bin/csh
/bin/dash
/bin/ksh
/bin/sh
/bin/tcsh
/bin/zsh
```

## Mettre à jour `bash`

```bash
homebrew install bash
```

`homebrew` va installer la dernière version du `bash` alors que **Apple** pour des raisons de licence reste sur la version 3

`homebrew` installe `bash ` dans le dossier `/usr/local/Cellar`

il existe un lien symbolique dans `/usr/local/bin` :

```bash
ls -la /usr/local/bin

# ...
lrwxr-xr-x    1 kar   admin        30 Jan 11 15:50 bash -> ../Cellar/bash/5.0.11/bin/bash
```

On doit l'ajouter dans `/etc/shells` les `shell` possible par défaut :

```bash
kar : ~ $ cat /etc/shells
# List of acceptable shells for chpass(1).
# Ftpd will not allow users to connect who are not using
# one of these shells.

/bin/bash
/bin/csh
/bin/dash
/bin/ksh
/bin/sh
/bin/tcsh
/bin/zsh
/usr/local/bin/bash
```

Pour éditer le fichier il faut le `sudo`

```bash
sudo nano /etc/shells
```

ensuite la commande pour établir le `shell` par défaut

```bash
chsh -s /usr/local/bin/bash
```

`-s` pour `shell`

## Changer de `shell`:

### `chsh -s /bin/zsg`