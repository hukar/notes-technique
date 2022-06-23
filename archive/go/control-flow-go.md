# boucle

une seule boucle `for` pour tous les diriger

```go
a := 'c'

for a < 'g' {
	fmt.Printf("%c\n",a)
	a++
}

> c
> d
> e
> f
```

identique à `while`

## boucle infinie

```go
for {
	a++
	if a%2 == 0 {
		continue
	}
	
	fmt.Printf("%c\n",a)

	if a >= 'j' {
		break
	}
}

> e
> g
> i
```

# conversion de type

```go
fmt.Println([]byte("hello, coco :)"))

> [104 101 108 108 111 44 32 99 111 99 111 32 58 41]
```

on converti une chaîne de caractère en tableau de byte (int8).

## affichage unicode utf8

```go
fmt.Printf("%v - %v - %v\n",i,string(i),[]byte(string(i)))

5002 - ᎊ - [225 142 138]
5003 - ᎋ - [225 142 139]
5004 - ᎌ - [225 142 140]
5005 - ᎍ - [225 142 141]
5006 - ᎎ - [225 142 142]
5007 - ᎏ - [225 142 143]
```

## afficher les hièroglyphes

```go
for i := '\U00013000' ; i < '\U0001342F'  ;i++ {
	fmt.Printf("%U - %v - %v\n",i,string(i),[]byte(string(i)))
}
	
U+13261 - 𓉡 - [240 147 137 161]
```

sur deux octets : `'\u3456'`  
sur quatre octets : `'\U09A2346B'` avec u majuscule

<p style="font-size:94px;text-align:center"> 𓉡 </p>

## chaîne de caractère = tableau de caractères

```go
mot := `pipi caca`

fmt.Println([]rune(mot))
fmt.Println([]byte(mot))
fmt.Println(len(mot))

[112 105 112 105 32 99 97 99 97]
[112 105 112 105 32 99 97 99 97]
9  // len nous donne le nombre de caractère
}
```

## différence entre "" et ``

format brute : pas de caractère d'échappement => ``

```go
mot := `pipi caca \n pipi popo`

fmt.Println([]rune(mot))
fmt.Println(len(mot))
fmt.Println(mot)
	
> [112 105 112 105 32 99 97 99 97 32 92 110 32 112 105 112 105 32 112 111 112 111]
> 22  // car \ et n font deux caractères
> pipi caca \n pipi popo


mot = "pipi caca \n pipi popo"

fmt.Println([]rune(mot))
fmt.Println(len(mot))
fmt.Println(mot)

> [112 105 112 105 32 99 97 99 97 32 10 32 112 105 112 105 32 112 111 112 111]
> 21  // car \n vaut un caractère
> pipi caca 'saut de ligne \n'
 pipi popo
```

# switch

```go
switch "h" {
case "coco":
	fmt.Println("alo coco")
case "mimi":
	fmt.Println("alo mimi")
default:
	fmt.Println("alo yo bo!!")
}

> alo yo bo!!
```

Pas besoin de break, il est par défault.

## fallthrough exécuter le suivant

```go
switch "coco" {
case "coco":
	fmt.Println("alo coco")
	fallthrough
case "mimi":
	fmt.Println("alo mimi")
	fallthrough
case "baba":
	fmt.Println("alo baba")
	fallthrough
case "glu":
	fmt.Println("alo glu")
default:
	fmt.Println("alo yo bo!!")
}

> alo coco
> alo mimi
> alo baba
> alo glu
```
### multiple entrée

```go

switch "glu" {
case "coco", "mimi":
	fmt.Println("alo cocomimi ")
case "baba", "glu":
	fmt.Println("alo baba glu")
default:
	fmt.Println("alo yo bo!!")
}

> alo baba glu
```

## switcher le type

```go
type Contact struct {
	greeting string
	name     string
}

func SwitchOnType(x interface{}) {

	// X.(type) is an assertion
	switch x.(type) {
	case int:
		fmt.Println("type is int")
	case string:
		fmt.Println("type is string")
	case rune:
		fmt.Println("type is rune")
	case Contact:
		fmt.Println("type is Contact")
	default:
		fmt.Println("type is unknown")
	}
}

func main() {
	a := "pipi"
	b := '5'
	var c = Contact{"hello", "vivi"}

	SwitchOnType(a)
	SwitchOnType(b)
	SwitchOnType(c)
}

> type is string
> type is rune
> type is Contact
```

## switcher une expression

```go
var a uint = 67
b := a + 7
switch {
	case a == 45:
		fmt.Println("something")
	case a > 45 && b > 45:
		fmt.Println("else")
		fallthrough
	default:
	fmt.Println("youpi")	
}

> else
> youpi
```

# if

## initialisation dans le if

```go
if a := 56; a != 0 {
	fmt.Println(a)  // ici a est défini
}

fmt.Println(a)  // ici a n'est pas défini
```

# formatage en masse

```sh
go fmt ./...
```

formatage de tous les fichiers dans tous les répertoire de votre répertoire courant