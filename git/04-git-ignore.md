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



