# Annexe A Créer un dépôt sauvegarde

(Par exemple en branchant une sauvegarde externe)

## 1 - créer un dépôt

#### Dans le support de sauvegarde

`/Usb/depot-sauvegarde`

```bash
mkdir hukar-local-repositoty.git

cd hukar-local-repositoty.git/
git init --bare
Initialized empty Git repository in /Users/kms/Desktop/hukar-local-repositoty.git/
```

## 2 - l'ajouter dans son projet comme remote

#### Dans le répertoire de travail

`/Travail/depot-local`

```bash
git remote add backup /Users/kms/Desktop/hukar-local-repositoty.git
git push backup master
```

On ajoute ici le `remote` et on `push` dedans.

## 3 - Restaurer son répertoire de travail ailleurs 

#### On se positionne sur un répertoire vide

`/Ailleur/depot-recuperation`

```bash
cd depot-recuperation/

git clone /Users/kms/Desktop/hukar-local-repositoty.git
Cloning into 'hukar-local-repositoty'...
done.
```

## 4 - commandes utiles

#### `df -h` 

affiche les disques et leur chemin

#### `git init --bare` 

initialise un dépôt nu (sans répertoire de travail), cela permet de gagner de la place.

Un dépôt nu est identique au contenue de `.git`

####  `git remote add [alias] [target]` 

ajoute un dépôt distant et lui lie un alias.

```bash
git remote
backup
origin

git remote -v
backup  /Users/kms/Desktop/hukar-local-repositoty.git (fetch)
backup  /Users/kms/Desktop/hukar-local-repositoty.git (push)
origin  https://github.com/hukar/udemy-git-two.git (fetch)
origin  https://github.com/hukar/udemy-git-two.git (push)
```

#### `git remote` 

affiche la liste des dépôts distants

#### `git remote -v` 

affiche la liste détaillée

## 5 - renommer le dépôt

Le nouveau dépôt créer a comme remote le dépôt de sauvegarde, mais la commande clone en a fait le dépôt `origin`.

On va le renommer `backup`.

`/Ailleur/depot-recuperation`

```bash
git remote -v
origin	/Users/kms/Desktop/hukar-local-repositoty.git (fetch)
origin	/Users/kms/Desktop/hukar-local-repositoty.git (push)

git remote rename origin backup

git remote -v
backup	/Users/kms/Desktop/hukar-local-repositoty.git (fetch)
backup	/Users/kms/Desktop/hukar-local-repositoty.git (push)
```

#### `git remote rename [old name] [new name]`

## 6 - modifier l'url

#### `git remote set-url NAME NEWURL`

```bash
git remote set-url shared //raadvst-consetat.be/shared/infcel/aum-kms-equipe-web/git-repos.git
```
