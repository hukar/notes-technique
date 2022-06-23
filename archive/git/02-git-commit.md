# 02 git Commit

dossier travail + dossier .git -> repository git

## théorie

Git fonctionne avec trois zones :

1. Dossier de travail
2. Zone d'index
3. Dépôt local

###Pour transformer un dossier en repository :

```bash
git init
```

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/git-init.png)

### Pour ajouter des modification dans l'index

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/git-indexation.png)

### Pour en retirer

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/git-desindexation.png)

### Pour enregistrer les modifications dans le repository

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/git-commit.png)

## Initialiser un dépôt `git init`

## Vérifier son dépôt `git status`

```bash
git status
```

```bash
Untracked files:
  (use "git add <file>..." to include in what will be committed)

	hello.html
	style.css
```

Deux fichiers non suivi repérés (Untracked files)

## Indexer ses fichiers `git add`

La commande `add` permet d'ajouter à l'index des fichiers et des dossiers (contenant des fichiers).

```bash
git add titi.html zozo.css
```

pour indexer tous les fichiers :

```bash
git add .
git status
```

```bash
On branch master

No commits yet

Changes to be committed:
  (use "git rm --cached <file>..." to unstage)

	new file:   hello.html
	new file:   style.css
	new file:   tata/tonton.txt
	new file:   tata/zozo.css
	new file:   toto/pipi.txt
```

## Désindexer des fichiers `git reset`

```bash
git reset toto/pipi.txt
git status
```

```bash
On branch master

No commits yet

Changes to be committed:
  (use "git rm --cached <file>..." to unstage)

	new file:   hello.html
	new file:   style.css
	new file:   tata/tonton.txt
	new file:   tata/zozo.css

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	toto/
```

## *commiter* dans le dépôt local `git commit`

```bash
git commit -m "mon premier commit"
```

`-m` pour ajouter un message

## Voire les changements `git diff`

```bash
git diff
```

```bash
diff --git a/hello.html b/hello.html
index ada4889..b88521b 100644
--- a/hello.html
+++ b/hello.html
@@ -13,7 +13,7 @@
        <body>
 
                <!-- TITRE -->
-               <div>
+               <div id="top-banner">^M
                        
```

On voie que la ligne - a été supprimer et la ligne + ajouter.

Git travaille avec des lignes entière, **il y a seulement ajout ou suppression de ligne**.

Si on ajoute le fichier modifié à l'index :

```bash
git add hello.html
```

Un `git diff` n'affiche plus rien

Si on veut quand même voire les différence avec le dernier `commit` :

```bash
git diff --cached
```

On retrouve le résultat précédent :

```bash
diff --git a/hello.html b/hello.html
index ada4889..b88521b 100644
--- a/hello.html
+++ b/hello.html
@@ -13,7 +13,7 @@
        <body>
 
                <!-- TITRE -->
-               <div>
+               <div id="top-banner">^M
                        Bienvenue dans cette formation GIT !
                </div>
```

