# package

un package est un dossier dans lequel les fichiers commencent par `package nomdudossier` pour faire partie du package

`func minuscule(s string) string { ... }` pour une fonction privée au scope du package, c'est à dire les fichiers du package

-> première lettre en minuscule

`func Majuscule(i int) string { ... }` pour une fonction public, c'est à dire accessible dans le fichier main lorsque le package est importé

-> première lettre en majuscule

De même pour les variables :

```go
var name = "jean" // privée
var Name = "jean" // public
```

## go install

installe des fichiers archive dans le répertoire pkg

`/Users/hukar/go/pkg/darwin_amd64/github.com/udemy-training/05_package`

dedans on trouve stringutil.a (.archive) 

Les archives accélère la compilation du code car la librairie est importée depuis ces fichiers

# variables
```go
fmt.Printf("type %T valeur : %v", a, a)
```

`%v` affiche la donnée dans son format par défaut
`%T` affiche le type de la donnée

```go
a := "string // assignation raccourcie
var a string = "string" // forme longue

var a int // vaut 0
var b string // vaut ""
var c bool // vaut false
```