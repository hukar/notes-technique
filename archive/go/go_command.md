# goimports

Cette commande permet de corriger les import manquants et de réindenter le code

```sh
goimports test.go
package main

import "fmt"

func main() {
	fmt.Println("bonjour")
}
```
Va afficher le résultat seulement dans le terminal sans pour autant modifier le fichier

```sh
goimports -w test.go
```

modifie le fichier