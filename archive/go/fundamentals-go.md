# Les Bases

## affichage formaté Printf

```go
fmt.Printf("%d - %b\n",42,42)
> 42 - 101010
```

* %d décimal
* %b binaire
* %x hexadécimal
* %#x hexadécimal avec l'écriture 0x
* %q caractère UTF8

```go
fmt.Printf("%d \t %b \t %x \t %#x\n",42,42,42,42)
> 42       101010          2a      0x2a
```

## la boucle for 

```go
for i := 0;i < 40;i++ {
	...
}
```

# Closure

```go
func main() {
	x := 8
	y := 89

	{
		y := 7
		fmt.Println(x)
		fmt.Println(y)
	}

	fmt.Println(y)
}

> 8
> 7
> 89
```

scope 1 => scope 2
scope 1 <> <= scope 2

Le scope englobant est accessible dans les scopes englobé, mais pas l'inverse

```go
func main() {
	incrementMaker := func() func() int {
		x := 0
		return func() int{
			x++
			return x
		}
	}

	myInc := incrementMaker()
	fmt.Println(myInc())
	fmt.Println(myInc())

	mySecondInc := incrementMaker()
	fmt.Println(mySecondInc())
	fmt.Println(mySecondInc())
}

> 1
> 2
> 1
> 2
```

Le contexte de création d'une fonction est conservé après l'appel de la fonction-contexte : x continue d'exister après incrementMaker()

# l'underscore
l'identifiant blanc permet de ne pas utiliser une variable déclarée sans générer une erreur

## récupérer une page web 

```go
import
(
	"fmt"
	"net/http"
	"io/ioutil"

)

func main() {
	res, _ := http.Get("http://benalman.com/news/2010/11/immediately-invoked-function-expression/")
	page, _ := ioutil.ReadAll(res.Body)
	res.Body.Close()
	fmt.Printf("%s",page)
}
```

# constante et iota

déclarer un constante

```go
const st string = "something..."
// ou bien
const st = "something..."
```

déclarer plusieurs constantes

```go
const (
		A = iota
		B = iota
		C = iota
	)
fmt.Println(A,B,C)

0 1 2

// autre syntaxe
const (
		A = iota
		B 
		C 
	)
fmt.Println(A,B,C)

0 1 2
```

`iota` c'est un petit incrément automatique

```go
const (
		_  = iota
		KB = 1 << (iota * 10)
		MB = 1 << (iota * 10)
	)

fmt.Printf("%b \t %d \n", KB, KB)
fmt.Printf("%b \t %d \n", MB, MB)

> 10000000000 	 1024 
> 100000000000000000000 	 1048576 
```

`<<` opérateur de shift

`1 << (iota*10) // iota*10 = 10` je décale le bit 1 de 10 place

# Pointeurs

```go
var a int = (34.0 + 0i) + 9

fmt.Printf("%T - %v - %p", a, a, &a)

> int - 43 - 0xc42000e1f8
```

## utilisation avec fmt.Scan

```go
var metters int
	fmt.Println("combien de mètres vous nagez ?")
	fmt.Scan(&metters)
	if metters == 1 {
		fmt.Printf("%d mètre", metters)
	} else {
		fmt.Printf("%d mètres !!", metters)
	}
```

`fmt.Scan`reçoit l'adresse de la variable dans laquelle sera stocké l'entrée de l'utilisateur.

```go
a := 67

fmt.Println(a)
fmt.Println(&a)

b := &a

fmt.Println(b)
fmt.Println(*b)

> 67				// valeur de a
> 0xc42000e0d0	// adresse de a
> 0xc42000e0d0	// valeur de b
> 67				// valeur à l'adresse contenue par b => *b
```
`b` est une référence à une adresse mémoire  
`*b` dé-référence cette adresse

```go
*b = 'x'
fmt.Printf("%c\n",a)

> x
``` 
changer la valeur à l'adresse contenue dans b change la valeur de a
b et a référence la même place en mémoire

