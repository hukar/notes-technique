# 01 getting started

Sur ma machine, MongoDB se trouve :

```bash
🦄 ~ which mongo
/usr/local/bin/mongo

🦄 ~ which mongod
/usr/local/bin/mongod
```

## vérifier que `mongod` tourne

```bash
🦄 ~ ps aux | grep mongo
kar               1267   0.1  0.1  5599328  11200   ??  S    10Apr20  52:17.70 /usr/local/opt/mongodb-community/bin/mongod --config /usr/local/etc/mongod.conf
```

Procédure ici :

https://docs.mongodb.com/manual/tutorial/install-mongodb-on-os-x/

## Installer avec `brew`

1 . 

```bash
brew tap mongodb/brew
```

2.

```bash
brew install mongodb-community@4.2
```

`brew` installe les fichiers binaires et :

Le fichier de configuration : `/usr/local/etc/mongod.conf`.

Le répertoire des logs : `/usr/local/var/log/mongodb`.

Le répertoire des données : `/usr/local/var/mongodb`.

## Lancer `mongod` comme macOS service

```bash
brew services start mongodb-community@4.2
```

### l'arrêter

```bash
brew services stop mongodb-community@4.2
```

### lister ses services

```bash
🦄 ~ brew services ls
Name              Status  User Plist
mongodb-community started kar  /Users/kar/Library/LaunchAgents/homebrew.mxcl.mongodb-community.plist
```

