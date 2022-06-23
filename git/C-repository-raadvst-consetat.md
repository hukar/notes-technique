# création des repository pour raadvst-consetat sur le disque w

Le disque `w` est un disque distant auto-monté dans le dossier `mnt/ressources`.

Les `remote repository` sontcréé sur `w`.

## Arborescence sur les machine de travail

`raadvst-consetat` -|

​                                     |-`shared`

​                                     |-`juridict`

​                                     |-`dashboard`

​                                     |-`eproadmin`

`shared` contient la documentions (ou autre) partagé.

Chaque application a son répertoire :

ex : `juridict` -|

​                           |-`.git`

​                           |-`code`

​                           |-`technical-doc`

​                           |-`documents`

​                           |-`autres ...`

## Arborescence sur `w`

`raadvst-consetat` -|

​                                     |-`shared.git`

​                                     |-`juridict.git`

​                                     |-`dashboard.git`

​                                     |-`eproadmin.git`

## Commandes bash :

```bash
kms: raadvst-consetat $ pwd
/mnt/Resources/w/aum-kms/raadvst-consetat
```

Le terminal est ouvert directement dans le répertoire racine.



```bash
sudo rm -rf ./*
```

Pour nettoyer le répertoire.



```bash
mkdir {shared,juridict,dashboard,eproadmin}.git
```

Pour créer les quatres `remote`.

Les accolades fonctionnent comme *"une boucle"*



```bash
 for repo in *
> do
> cd $repo
> git init --bare
> cd ..
> done
```

Initialisation des repos *"nus"*

## Ensuite dans les répertoires de travail de sa machine :

Pour chaque `repository` :

```bash
# Dans le dossier de travail, ici dashboard
git init
git add .
git commit -m "first commit"
git remote add origin /mnt/Resources/w/aum-kms/raadvst-consetat/dashboard.git
git push origin master
```

Attention un `git push` pour le premier `push ne fonctionnera pas` :

```bash
kms: dashboard $ git push
fatal: The current branch master has no upstream branch.
To push the current branch and set the remote as upstream, use

    git push --set-upstream origin master
```



Il faut explicitement spécifier `git push origin master`