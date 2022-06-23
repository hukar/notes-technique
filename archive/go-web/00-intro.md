# 00 Intro

## Fonctions

```
func (receveur) identifiant(paramètres) (types retour) { code }
```

```go
type Titi struct {
	name string
	age  int
}

func (t *Titi) manger(meat string) string {

	return "je mange du " + meat
}

func main() {

	toto := Titi{"toto", 67}

	fmt.Println(toto.manger("concombre"))
}
```

## Struct, Map et Slice

```go
titi := struct {
            age  int
            name string
        }{67, "nini"}

slicex := []float64{4.0, 5.8, 6.0, 7.8}

mapx := map[string]int{"renée": 2, "michaël": 8}

toto := Titi{"toto", 67}
```

## Composition et non héritage

On peut déclarer un type dans un autre.

```go
type hero struct {
	name string
	pawa int
}

type superhero struct {
	fly   bool
	speed int
	hero
}
```

On peut associer une méthode à un type

```go
func (sh *superhero) presentation() {
    fmt.Printf("my name is %s\n", sh.hero.name)
	fmt.Printf("hello Can I fly ? %t\n", sh.fly)
	fmt.Printf("my speed is %d and my pawa is %d\n", sh.speed, sh.pawa)
}
```

```go
nova := superhero{true, 123, hero{"nova", 5000}}

nova.presentation()
```

```sh
my name is nova
hello Can I fly ? true
my speed is 123 and my pawa is 5000
```

Il est possible d'accéder directement au propriété d'un type qui en compose un autre:

```go
fmt.Printf("my name is %s\n", sh.hero.name)
// équivalent à :
fmt.Printf("my name is %s\n", sh.name)
```

## Polymorphisme

```go
type Person struct {
	name string
	age  int
}

type SecretAgent struct {
	Person
	licenceToKill bool
}

func (p *Person) speak() {

	fmt.Printf("hello I'm %s and I've %d\n", p.name, p.age)
}

func (sa *SecretAgent) speak() {

	fmt.Printf("Hi I'm %s, I've %d, I'm secret my licence is: %t\n", sa.name, sa.age, sa.licenceToKill)
}
```

```go
titi := Person{"titi", 45}
titi.speak()

toto := SecretAgent{Person{"toto", 56}, true}
toto.speak()
toto.Person.speak()
```

```sh
hello I'm titi and I've 45
Hi I'm toto, I've 56, I'm secret my licence is: true
hello I'm toto and I've 56
```

On peut appeler la fonction de l'*ancêtre* en le désignant littéralement `toto.Person.speak()` 

## Interfaces

> #### Je veux un argument qui implémente cette méthode !

syntaxe :

```go
type stuff interface{
    oneMethod() int
    otherMethod(int) string
    legacyFromOtherInterface
}
```

Base :

```go
type rock struct {
	mass   int
	volume int
}

type geode struct{}

type dense interface {
	density() int
}

func (r rock) density() int {
	return r.mass / r.volume
}

func (g geode) density() int {
	return 100
}

func isItDenser(a, b dense) bool {
	return a.density() > b.density()
}

func main() {
	g := geode{}
	r := rock{10, 1}
	
	fmt.Println(isItDenser(g, r))
}
```

On utilise l'interface dans la méthode `isItDenser` :

```go
func isItDenser(a, b dense) bool {
	return a.density() > b.density()
}
```

#### ! attention on ne peut pas associer d'attribut avec les interfaces, seulement des méthodes

```go
func isItDenser(a, b dense) bool {
	fmt.Println(a.mass)
	return a.density() > b.density()
}
```

```sh
prog.go:30:15: a.mass undefined (type dense has no field or method mass)
```

### Héritage d'interface

```go
type MagicalFire interface{
	fireBall()
}

type FireFighter interface{
	MagicalFire
	getPowa() int
}
```

Créons deux types qui les implémentent :

```go
type Magicien struct{
	name string
	powa int
}

type Dragon struct{
	color string
	powa int
}

func (d Dragon) fireBall() {
	fmt.Println(d.color, d.powa, "fluuush")
}

func (m Magicien) fireBall() {
	fmt.Println(m.name, m.powa, "fire !!!")
}

func (d Dragon) getPowa() int {
	return d.powa
}

func (m Magicien) getPowa() int {
	return m.powa
}
```

On peut déclarer des paramètres de ces deux interfaces :

```go
func fireDemonstration(a, b MagicalFire) {
	a.fireBall()
	b.fireBall()
}

func fireFighting(a,b FireFighter) {
	if (a.getPowa() > b.getPowa()) {
		a.fireBall()
	} else {
		b.fireBall()
	}
}
```

On écrit `getPowa()` et pas `powa()` car un attribut et une méthode ne peuvent pas avoir le même nom

Démonstration :

```go
drag := Dragon{"green", 5700}

mag := Magicien{"aznael", 4678}

fireDemonstration(mag, drag)
fireFighting(mag, drag)
```

```sh
aznael 4678 fire !!!
green 5700 fluuush

green 5700 fluuush
```

## Interface vide `type a interface{}`

L'interface vide permet à une fonction d'accepter n'importe quel type :

```go
type a interface{}

func showSomething(something a) {
	fmt.Println(something)
}
```

Testons le programme :

```go
var s string = "hello"
var i int = 45
var f float64 = 4.78

showSomething(s)  // hello
showSomething(i)  // 45
showSomething(f)  // 4.78
```

##Constante : `cons`

```go
func showInt(nb *int) {
	fmt.Println(*nb)
}

func main() {
	var a = 6
	const b = 7
	
	showInt(&a) // 6
	
	// showInt(&b)
}
```

maintenant la même chose avec une constante :

```go
showInt(&b)  // cannot take the address of b
```

### Utilisation du mot `iota`

```go
const (
	_ = 1 << (10*iota)
	KiB  // 1024
	MiB  // 1048576
	GiB  // 1073741824 
)
```

Apparement les plus grands nombre lève une erreur

```go
TiB // constant 1099511627776 overflows int
```

