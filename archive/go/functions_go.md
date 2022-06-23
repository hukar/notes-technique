# Sprint

string print

```go
func main() {
	fmt.Println(greet("yoyo", "tutu"))
}


func greet(fname, lname string) string {
	return fmt.Sprint(fname, lname)
}
```

`fmt.Sprint` prends autant d'arguments que l'on veut, les concatène et renvoie un string.

# valeur retour de fonction

retourner une variable pré-définie dans la éclaration de fonction

```go
func greet(fname, lname string) (s string) {
	s = fmt.Sprint(fname, lname)
	return
}   // retourne la valeur de s
```

retour de plusieurs valeurs

```go
func main() {
	fmt.Println(greet("yoyo", "tutu"))
}

func greet(fname, lname string) (string, string) {

	return fmt.Sprint(fname, lname), fmt.Sprint(lname, fname)
}

> yoyotutu tutuyoyo
```

ET un mix des deux

```go
func greet(fname, lname string) (s1 string, s2 string) {
	s1 = fmt.Sprint(fname, lname)
	s2 = fmt.Sprint(lname, fname)
	return
}
```

# variadic

Une fonction pouvant recevoir un ou plusiseurs paramètre :  
`... type`

```go
func testVariadic(sf ...int) int {
	fmt.Println(sf)
	fmt.Printf("%T\n", sf)
	s := 0

	for i, v := range sf {
		s += v * i
		fmt.Printf("%d += %d*%d\n", s, v, i)
	}
	return s / len(sf)
}

fmt.Printf("la moyenne est : %d\n", testVariadic(1, 2, 3, 4))

> [1 2 3 4]
> []int
> 0 += 1*0
> 2 += 2*1
> 8 += 3*2
> 20 += 4*3
> la moyenne est : 5
```

envoyer et recevoir un nombre de paramètres indéfini :

```go
func main() {
	data := []float64{4, 6, 8}
	//data = append(data,12,14,16)
	fmt.Println(data)
	testPoint(data...)
}

func testPoint(s ...float64) {
	for i, v := range s {
		fmt.Printf("i: %d a : %f\n",i, v)
	}
}

> [4 6 8]
> i: 0 a : 4.000000
> i: 1 a : 6.000000
> i: 2 a : 8.000000
``` 

`data...` signifie ouvrir le slice passer d'un item à plusieurs

# fonction expression

une expression de fonction est l'assignement à une variable une fonction anonyme.

```go
func main() {
	func test () {
		fmt.Println("hello coco")
	}
}

// erreur c'est impossible
```

On ne peut déclarer une fonction dans une autre fonction  
On doit utiliser une expression de fonction

```go
func main() {
	test := func() {
		fmt.Println("hello coco")
	}

	test()
}

// c'est bon, cela fonctionne
```

# rappel des closures et fonction anonyme

```go
func testClosure(s string) func() {
	p := s
	return func() {
		fmt.Println(p)
	}
}

func main() {
	secret01 := testClosure("premier secret")
	secret02 := testClosure("deuxième secret")

	secret01()
	secret02()
}

> premier secret
> deuxième secret
```

`func testClosure(s string) func()`  
fonction testClosure prenant une chaîne de caractère en argument et retournant une fonction anonyme

