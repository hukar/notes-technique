# Webstorm

commande :

```sh
/usr/local/bin/webstorm
```

`cmd + click` sur une fonction d'un package et on ouvre le fichier source.

# commandes

un tiret pour une lettre

deux tirets pour le mot

```sh
-u
--update
``` 

# hello world

`fmt.Println` retourne le nombre de byte

go run -> fait tourner le programme comme un script

go build -> produit un exécutable

```sh
go build main.go
./01_helloWorld # le nom du dossier

hello world!
```

Sous mac ./ permet de lancer un exécutable

# commandes Go

```sh
go clean #efface le fichier exécutable
go install #compile et met le fichier dans 
		   #le dossier bin de son workspace
```

mettre à jour depuis un repository github

```sh
go get -u github.com/
```

# git et github

`git init` commencer un dépot

**.gitignore** le fichier des exceptions (fichiers à ne pas versionner)

`git status` donne l'état des lieux
`git add --all` ajoute tous les fichiers à être commités

## se connecter à github

créer un repository sur github ayant le même nom que votre dossier local.

github fournit le code suivant :

```sh
echo "# udemy-training" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/hukar/udemy-training.git
git push -u origin master
```