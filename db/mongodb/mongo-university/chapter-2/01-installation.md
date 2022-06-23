# 01 `MongoDB` le langage de requête

# Installation

## Les clients

`mongodb compass` est un client graphique.

Il ne supporte pas encore tout le langage de requête.

`mongodb shell` est un client texte

## Installation

`mongodb.com/download-center/`

**Télécharger MongoDB Enterprise Server**

Dézipper le dossier et le placer à la racine de son `home`.

Modifier `.bash_profile`

```bash
PS1="\u : \W $ "

# Setting PATH for Python 3.8
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.8/bin:${PATH}"

# Setting PATH for MongoDB 4.2.3
PATH="/Users/kar/mongodb-macos-x86_64-enterprise-4.2.3/bin:${PATH}"

export PATH
```

Mettre à jour `PATH`

```bash
source .bash_profile
```

Rafraîchi le `shell`  avec le fichier `.bash_profile`

## Ouvrir le shell `MongoDB`

```bash
mongo --nodb
```

