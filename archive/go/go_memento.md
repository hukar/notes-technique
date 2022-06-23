# argument du programme

```go
import "os"

if len(os.Args) > 1 {
	fmt.Println(os.Args[1])
}
```

# run, build et install

```sh
go run main.go
// lance le programme sans le compiler

go build main.go
// compile le programme en main
./main
// lance le programme

go install
// crée une commande du nom de dossier du main dans le répertoire bin du GOPATH => on crée une commande disponnilble dans le shell
```

on peut soi-même créer une commande en mettant le fichier binaire et en lui attribuant __les droits en éxecution__



# variable

```go
var mavar int
mavar = 8
// ou

mavar := 8

// := opérateur type inférence

var tabchaine []string
tabChaine = os.Args // os.Args étant le tableau des arguments transmis au programme
```

# time

```go
hour := time.Now().Hour()
```

# Fonction

```go
func MaFonction (mavar int) string {

}
```

# Loop

```go
for i := 0;i < 6; i++ {
	fmt.Println(i)
}
```

avec juste la condition

```go
i := 0
flag := true

for flag {
	if i > 5 {
		flag = false
	}
	
	fmt.Println(i)
	i++
}
```

juste for

```go
i := 0

for {
	if i > 5 {
		break
	}
	
	fmt.Println(i)
	i++
}
```

# Array

simple tableau

```go
 var tab [5]int
 
 tab[0] = 564
 tab[1] = 32
 ...
 tab[4] = 170
```
Ne peux avoir plus de 5 éléments

La signature d'une fonction doit déclarer la taille du tableau en arguments

```go

func faireQuelqueChose (tab [6]string) int {
	...
}
```

tableau _'slice'_

```go
var tab []string

tab = append(tab,"une chaine")
tab = append(tab,"une autre chaine")
```
Ce genre de tableau peux avoir autant d'éléments qu'on veut.

Déclarer une 'slice' littérale :

```go
tab := []int{1,4,78,9,3}
```

# clause range

```go
for i := range tab {
	fmt.Println(tab[i])
}
```

Fonctionne avec un tableau classique ou un 'slice'

### avec un slice

```go
for i, element := range tab {
	fmt.Println(i)
	fmt.Println(element)
}
```

Si on utilise pas l'index i on peut utiliser l'underscore pour stipuler à go que l'on ne se sert pas de cette valeur

```go
for _, element := range tab {
	fmt.Println(element)
}
```

# Struct

Les structures se déclare en dehors du main

```go
type Perso struct {
	name string
	age int
}
```

Puis dans le main

```go
Gerard := Perso{"gerard",27}
```

## fonction associé (méthode)

```go
type Perso struct { ... }
// à l'extérieur du main
func (p Perso) vieux()string {
	if p.age > 69 {
		return "je suis vieux"
	} else {
		return "je ne suis pas vieux"
	}
}

// puis à l'intérieur du main

Michel := Perso{"michel",45}
fmt.Println(Michel.vieux())
```

func __(p Perso)__ associe la fonction à une structure

Variante explicite

```go
Michel := Perso{name: "michel",age: 56}
```

# pointeur

```go
mot := "mot"
autreMot := &mot

fmt.Println(&mot)
fmt.Println(autreMot)
// le contenu de l'adresse : la valeur pointé
fmt.Println(*autreMot)

0x1040c108
0x1040c108
mot

```

## un pointeur en argument de fonction

```go
func rajeunir(g *Gogo) {
  g.age -= 10;
  fmt.Println(g)
}
```

# interface

1. créer un "slice" de struct

```go
type gopher struct {
	name string
	age int
}

// un tableau (slice) de pointeur vers gopher []*gopher
list := []*gopher{&gopher{name: "mimi",age: 45},&gopher{name: "dede",age: 67}} 

```

type interface

```go
type jumper interface{
	jump() string
}

```
à l'intérieur du type on a les prototypes de fonction

utilisation dans une liste et une fonction

```go
func getList() []jumper {
	list := []jumper{&gopher{name: "mimi",age: 45},&gopher{name: "dede",age: 67},&horse{name: "joli",weight: 345}} 
	return list
}
```

__[]jumper__ slice de type jumper (structure qui implémente jump())

# Package

![](/Users/hukar/Documents/notes-techniques/arborescence.png)

```go
/* main.go */

package main  // pas de guillemets

import (
  "fmt"
  "demo/monpack"  // chemin depuis src
)

func main () {

  fmt.Println(monpack.Salut("bobo")) // monpack. prefixe de package
}
```

```go
/* monpack/main.go
package monpack // prefixe de package


// nom des fonction première lettre en majuscule
func Salut(nom string) string{  
  return "salut copain : "+nom
}

```

# Parallelisme

## chronométré un programe

```sh
time go run main.go

name :  polo
name :  kirk
name :  boris
name :  margo

real	0m0.231s
user	0m0.173s
sys		0m0.069s
```

Code pour ralentir l'exécution:

```go

import (
  "fmt"
  "math"
)

func main () {
  names := []string{"marco","polo","kirk","boris","margo"}

  for _,n := range names {
    printName(n)
  }

}

func printName (n string) {

// cette parite ralentie artificielemnt l'exécution du code
  result := 0.0
  for i := 0; i < 100000000; i++ {
    result += math.Pi*math.Sin(float64(len(n)))
  }
  
  fmt.Println("name : ",n)
}
```

```sh
time go run main.go
real	0m7.797s
user	0m7.717s
sys		0m0.086s
```

Le code s'execute de manière séquentielle  
 printName() - printName() - printName()
 
 ### goroutine
 
 ```go
import (
  "fmt"
  "sync"
  "math"
)

func main () {
  names := []string{"marco","polo","kirk","boris","margo"}
  var wg sync.WaitGroup

  wg.Add(len(names))
  for _,name := range names {
    go printName(name,&wg)  // mot cle go
  }
  wg.Wait()

}

func printName (n string,wg \*sync.WaitGroup) {
  result := 0.0
  for i := 0; i < 100000000; i++ {
    result += math.Pi*math.Sin(float64(len(n)))
  }
  fmt.Println("name : ",n)

  wg.Done()
}
 }
 ```
on restreint à un suel coeur le test d'exécution : 

```sh
$ time GOMAXPROCS=1 go run main.go
real	0m7.811s
user	0m7.680s
sys		0m0.077s
```

Maintenant avec deux coeurs :

```sh
$ time GOMAXPROCS=4 go run main.go

// 4 processus = deux coeurs x deux tread
real	0m4.172s
user	0m11.322s
sys	0m0.078s
```

Par default go run va utiliser tout les coeurs disponnibles

On passe alors d'un traitement concurentiel à un traitement paralelle