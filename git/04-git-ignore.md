# Le fichier `.gitignore`

La liste des fichiers ignoré par git

Avant de remplir le fichier `.gitignore` on a :

```bash
git status
On branch master
Untracked files:
  (use "git add <file>..." to include in what will be committed)

        tmp/
```

Maintenant, je remplis le fichier `.gitignore` :

```bash
tmp/todo.txt

# tmp/*

# *.txt
```

Après un `git status` on a :

```bash
git status
On branch master
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

        modified:   .gitignore
```

Le dossier `tmp` n'apparaît plus.

#### ! `git add *` ne fonctionne pas avec les fichiers cachés, utiliser plutôt ` git add .` (répetoire courant) ou ` git add .gitignore`

## Retirer un répertoire du repository mais pas du file system

### `git rm -r --cached`

```bash
git rm -r --cached myDirectory
```



### Si on ajoute `.gitignore` en cours de projet

https://stackoverflow.com/questions/19663093/apply-gitignore-on-an-existing-repository-already-tracking-large-number-of-file

Si on a déjà un `repository` et que l'on veuille utiliser un fichier `.gitignore` voici une procédure :

```bash
git rm -r --cached .
```

Cela retire tout de l'index.

```bash
git add .
```

On ajoute maintenant de nouveau l'ensemble du projet mais cette fois `.gitignore` agit.

```bash
git commit -m ".gitignore is now working"
```



