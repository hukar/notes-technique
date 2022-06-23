04 git remote

## cloner un dépôt distant `git clone`

```bash
git clone https://github.com/hukar/my_web_site.git clone_mon_site
```

Si on ajoute pas le nom du dossier de clonage en dernier argument, git utilise le nom du dépôt distant.

On va configurer sans --global pour avoir un repère local :

```bash
git config user.name "clone_tata"
```

`.git/config`

```bash
# ...
[remote "origin"]
	url = https://hukar@bitbucket.org/hukar/testing_repository.git
	fetch = +refs/heads/*:refs/remotes/origin/*
[branch "master"]
	remote = origin
	merge = refs/heads/master
[user]
	name = clone_tata
```



## Gérer les remote

Un remote est un pointeur vers un autre dépôt.

git crée le `remote` `origin` lorsqu'on execute la commande `clone`

`.git/config` :

```bash
[core]
	repositoryformatversion = 0
	filemode = true
	bare = false
	logallrefupdates = true
	ignorecase = true
	precomposeunicode = true
[remote "origin"]
	url = https://hukar@bitbucket.org/hukar/testing_repository.git
	fetch = +refs/heads/*:refs/remotes/origin/*
[branch "master"]
	remote = origin
	merge = refs/heads/master
[user]
	name = clone_tata
```



##info sur un remote `git remote -v` et `git remote show`

```bash
git remote -v
```

```bash
origin  https://github.com/hukar/my_web_site.git (fetch)
origin  https://github.com/hukar/my_web_site.git (push)
```

`fetch` récupérer les modifications du serveur en local.

`push` on pousse du locale vers le serveur/dépôt distant.

```bash
git remote show origin
```

```bash
* remote origin
  Fetch URL: https://github.com/hukar/my_web_site.git
  Push  URL: https://github.com/hukar/my_web_site.git
  HEAD branch: (unknown)
  Local branch configured for 'git pull':
    master merges with remote master
```

## Ajouter un remote `git remote add`

On peut ajouter un remote à son dépôt :

```bash
git remote add pipistrelle https://github.com/hukar/my_web_site.git
```

On obtient donc un pointeur sur `origin` sur un dépôt distant.

## Supprimer un remote `git remote remove`

Pour supprimer un remote il suffit de taper :

```bash
git remote remove titi
```

alias : `git remote rm`

### Changer l'url `set-url`

```bash
git remote set-url nini http://monlapin.com
```



## Pousser ses commit `git push`

```bash
git push -u pipistrelle master
```

`git push -u <nom-du-remote> <nom-de-la-branche>`

Cela a pousser le code sur Github.

## Ajouter un tag vers le remote repository

```bash
git tag
```

```bash
ADD_GIST
COP_V_1
COP_V_2
```

#### ! un tag est associé a une release sur Github

```bash
git push neo COP_V_1
```

## Ajouter tous les tag

```bash
git push neo --tags
```

## Récupérer les mis-à-jours d'un remote repository

```bash
git fetch
```

rapatrie la position de `origin`

`master` reste sur le dernier `SHA-1`

```bash
git pull
```

Déplace `master` sur `origin`.

On peut faire juste `git pull` et `git fetch` sera exécuté automatiquement.