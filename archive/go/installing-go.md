# Installing Go


### sha1 checksum
sha1 algorythme de hachage

```sh
openssl sha1 /Users/hukar/Downloads/gcc-5.3-bin.tar 

SHA1(/Users/hukar/Downloads/gcc-5.3-bin.tar)= 3ba63bd23fa10099c837356d26fd6d30c544a3df
```

#### pour un sha256

```sh
shasum -a 256 /Users/hukar/Desktop/WebStorm-2017.2.dmg
```

### installer un nouveau package

on cherche un package sur godoc.

On récupère son identifiant github :
**github.com/nu7hatch/gouuid**

et puis dans le terminal on tape

```sh
go get github.com/nu7hatch/gouuid
```

`go get` est le gestionnaire de package

Afficher les variables d'environement :

```sh
env
```

Pour afficher les variables d'environement de Go :

```sh 
go env
``` 

```sh
GOARCH="amd64"
GOBIN=""
GOEXE=""
GOHOSTARCH="amd64"
GOHOSTOS="darwin"
GOOS="darwin"
GOPATH="/Users/hukar/go"
GORACE=""
GOROOT="/usr/local/go"
...
```

GOPATH -> workspace pour Go

GOROOT -> installation de Go

### configurer le PATH

Dans le fichier `hukar/.profile` ajouter :

```sh
PATH=$PATH:/Users/hukar/go/bin
```
	
### Précision sur le GOPATH

Le GOPATH est en fait composer de trois sous-repertoire `bin` `pkg` et `src`.

Si on précise le GOPATH sur un tel repertoire, les commande de type `go get -v github.com/capotej/groupcache-db-experiment/...` chargeront directement les bibliothèques extérieur dans le dossier `pkg`

Il suffit de configurer le GOPATH pour la session du terminal :

```sh
export GOPATH=monrepertoire/monworkspacegolang
```

Cette configuration s'effacera à la fermeture de la fenêtre du terminal