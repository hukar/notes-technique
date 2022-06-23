# Installation de go



[download](https://golang.org/dl/)

```sh
go version  /* donne la version de go installé */
go version go1.8.1 darwin/amd64
```

Créer un répertoire go/src dans son espace de travail

Définir l'espace de travail

```sh
export GOROOT=/usr/local/go
export GOPATH=/Users/hukar/go /* dans mon cas */
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin 
```

on défini deux nouveaux chemins dans la variable PATH  
$PATH étant le contenu de PATH

```sh
go run test.go 
// fait tourner le programme sans créer d executable 
go build test.go
// crée l exécutable test 
./test // lance l'exécutable test
``` 

```sh
echo PATH
/usr/local/bin:
/usr/bin:
/bin:
/usr/sbin:
/sbin:
/opt/X11/bin:
/usr/local/git/bin:
/usr/local/go/bin
```
echo $PATH me permet de voire tout les chemins où les commandes terminal vont être cherchée

J'ajoute au PATH le répertoire bin de mon workspace :

```sh
export PATH=$PATH:/Users/hukar/go/bin
```

Cet ajout est temporaire et sera effacé à chaque redémarage du terminal !

**Solution**  
créer un fichier .profile dans votre répertoire utilisateur chez moi /Users/hukar (~)

écrire dedans :

```bash
PATH=$PATH:/Users/hukar/go/bin
```

Pas besoin de `export` si la variable est déjà dans l'environnement