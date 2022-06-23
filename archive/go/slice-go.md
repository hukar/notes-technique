# Slice en GO

## pointeur sur slice

```go
// un slice initialisé avec trois valeurs
s := []int{1, 2, 3}

// un pointeur sur slice

pts := &s

*pts == s // true
(*pts)[0] == s[0] // true attention parenthèses obligatoires
```

## liste aléatoire d'int

utilisation du package `math/rand` et de la fonction Permut

```go
var n int = 56 // nombre d'éléments de la list
list := rand.Perm(n)

// [21 4 2 13 10 0 19 11 7 5 23 18 9 14 6 8 1 20 17 3 16 22 24 15 12]

// lire la liste

for i, v := range list {
	...
}
```

**note :** `v` n'est pas `list[i]` mais une variable local de valeur `list[i]`,si `v` est modifié, `list[i]` ne l'est pas

modifier la graine de génération aléatoire :

```go
// package time
rand.Seed(int64(time.Now().Second()))
```

## expression de slice

`s[start:finish]` start inclus finish exclu (premier inclus, dernier exclus)

```go
s := []string{"fraise","vanille","chocolat","framboise","citron"}
```

```go
a := s[1:5]
Printf("a : %v\n",a)
// a : [vanille chocolat framboise citron]

a := s[1:6]
Printf("a : %v\n",a)
// panic: runtime error: slice bounds out of range

a := s[1:]
Printf("a : %v\n",a)
// a : [vanille chocolat framboise citron]
	
b := s[3:3]
Printf("b : %v\n",b)
// de l'indice 3 à l'indice 3 exclu = rien
// b : []
	
c := s[:3]
Printf("c : %v\n",c)
// du début jusqu'à l'indice 3 exclu
// [fraise vanille chocolat]
```

## append

`func append(slice []Type, elems ...Type) []Type` 

**Rappel :** `...` ce sont les arguments variadic => une énumération, ou un égrainage

exemple :

```go
func show(s ...string) {
	for i,v := range s{
		Printf("i : %d v: %s\n",i,v)
	}
}

func main() {
	s := []string{"fraise","vanille","chocolat","framboise","citron"}
	
	show(s...)	
}
```
```
i : 0 v : fraise
i : 1 v : vanille
i : 2 v : chocolat
i : 3 v : framboise
i : 4 v : citron
```

Celà peut aussi être simplement un nombre de string séparé par une virgule :

```go
show("pipi","popo","pépé")
```

```
i : 0 v : pipi
i : 1 v : popo
i : 2 v : pépé
```

### coller deux slice

```
s1 : [fraise vanille chocolat framboise citron]
s1 : [fraise vanille framboise citron citron] // => ???
s2 : [fraise vanille framboise citron]
```

```go
s1 := []string{"fraise","vanille","chocolat","framboise","citron"}
	
s2 :=[]string{"pipi","popo","pépé"}
	
s3 := append(s1,s2...)
Printf("s3 : %v",s3)
```
```
s3 : [fraise vanille chocolat framboise citron pipi popo pépé]
```

## Supprimer un élément de slice avec append

Pour éviter une mutation non souhaitée du slice de départ il vaut mieux ré envoyer le résultat dans le slice de départ

```go
s1 := []string{"fraise","vanille","chocolat","framboise","citron"}
Printf("s1 : %v\n",s1)
	
//supprimer l'élément d'indice n = 2 => "chocolat"
s2 := append(s1[:2],s1[3:]...)
Printf("s1 : %v\n",s1)
Printf("s2 : %v\n",s2)
```
<div style="padding: 8px;color:#FF0070;background-color:#FFC6DF;border:1px solid #FF0070;">
Dans le cas où le type du tableau est un pointeur ou une struct contenant des pointeurs, il peut y avoir une fuite de mémoire.
</div>

[lien vers résolution](https://github.com/golang/go/wiki/SliceTricks)

## copy(dest, src []Type) int

Copy un tableau dans un tableau et renvoie la taille de dest.

### Si dest et src sont de même taille

on a un remplacement du contenu de `dest` par celui de `src` :

```go
t1 := []string{"michel","roger","nicolas","patrick"}
t2 := []string{"serpent","couleuvre","lézard","crocodile"}
	
copy(t1,t2)
	
Printf("t1 : %v t2 : %v\n",t1,t2)
Printf("len(T1) %v, cap(t1) %v\n",len(t1),cap(t1))
```

```
t1 : [serpent couleuvre lézard crocodile] t2 : [serpent couleuvre lézard crocodile]
len(t1) 4, cap(t1) 4
```

### Si dest est plus petit que src

```go
t1 := []string{"nicolas","patrick"}
t2 := []string{"serpent","couleuvre","lézard","crocodile"}
	
copy(t1,t2)
	
Printf("t1 : %v t2 : %v\n",t1,t2)
Printf("len(t1) %v, cap(t1) %v\n",len(t1),cap(t1))
Printf("adresse t1 : %p, adresse t2 : %p\n",&t1,&t2)
```
```
t1 : [serpent couleuvre] t2 : [serpent couleuvre lézard crocodile]
len(t1) 2, cap(t1) 2
adresse t1 : 0xc420086060, adresse t2 : 0xc4200860a0
```

### Et si dest est plus grand

`dest` n'a que ses premiers éléments remplacés 

```go
t1 := []string{"nicolas","patrick","maurice","flavio","michel"}
t2 := []string{"serpent","couleuvre","lézard","crocodile"}
	
copy(t1,t2)
	
Printf("t1 : %v t2 : %v\n",t1,t2)
Printf("len(t1) %v, cap(t1) %v\n",len(t1),cap(t1))
Printf("adresse t1 : %p, adresse t2 : %p\n",&t1,&t2)
```
```
t1 : [serpent couleuvre lézard crocodile michel] t2 : [serpent couleuvre lézard crocodile]
len(t1) 5, cap(t1) 5
adresse t1 : 0xc42000a0a0, adresse t2 : 0xc42000a0c0
```
## supprimer un élément de slice avec copy

On peut cibler une partie d'un slice dans laquelle on colle un autre slice :

```go
t2 := []string{"scarabée","escargot","limaces","fourmis"}
t3 := []string{"lion","girafe","zébu"}
	
copy(t2[3:],t3)
	
Printf("t2 : %v\n",t2)
```
```
[scarabée escargot limaces lion]
```

Le slice de départ conserve sa taille

Si le slice `src` est trop petit, on a un panaché des deux slice :

```go
t2 := []string{"scarabée","escargot","limaces","fourmis","ver","cigale","carabe","abeille"}
t3 := []string{"lion","girafe"}
	
copy(t2[2:],t3)
	
Printf("t2 : %v\n",t2)
```
```
t2 : [scarabée escargot lion girafe ver cigale carabe abeille]
```

[scarabée escargot `limaces` `fourmis` ver cigale carabe abeille]  
[`lion` `girafe`] => [scarabée escargot `lion` `girafe` ver cigale carabe abeille]

Maintenant d'un slice sur lui même :

```go
t1 := []string{"nicolas","patrick","maurice","flavio","michel"}
	
//supprimer patrick indice = 1
copy(t1[1:],t1[2:])
	
Printf("t1 : %v\n",t1)
```

t1[1:] = [patrick maurice flavio michel]  
t1[2:] = [maurice flavio michel]  
donc  
[nicolas `patrick` `maurice` `flavio` michel]  
[`maurice` `flavio` `michel`]  
on obtiens :  
[nicolas `maurice` `flavio` `michel` michel]

il ne reste plus qu'à supprimer le dernier élément

```go
t1 := []string{"nicolas","patrick","maurice","flavio","michel"}
	
	//supprimer patrick indice = 1
	copy(t1[1:],t1[2:])
	
	t1 = t1[:len(t1) - 1]
	
	Printf("t1 : %v\n",t1)
``` 
```
[nicolas maurice flavio michel]
```

Comme copy renvoie le nombre d'élément copiés on peut écrire la formule générale :

```go
// si i = 1 => 
// copy(t[i:], t[i+1:]) = len(t[i+1:]) = len(t) - i - 1
// et donc i + copy(t[i:], t[i+1:]) = i + len(t) -i -1 = len(t) - 1
t = t[:i+copy(t[i:], t[i+1:])]
```

Pour éviter des fuites mémoires on annule soi-même la valeur de l'élément à supprimer :

```go
copy(a[i:], a[i+1:])
a[len(a)-1] = nil // or the zero value of T
a = a[:len(a)-1]
```

> ## Code à prouver ??
> ```go
> 	a := "chien"
>	b := "singe"
>	c := "cochon"
>	
>	t := []*string{&a,&b,&c}
>	
>	Printf("départ : t : %v\n",t)
>	// var p *string = 0xc42000e1e0
>// écriture incompatible avec les pointeur 
>	var p *string = &b
>	// on supprime l'élément d'indice 1
>	
>	copy(t[1:], t[2:])
>	t[len(t)-1] = nil // or the zero value of T
>	Printf("etape 1 : t : %v\n",t)
>	t = t[:len(t)-1]
>	Printf("etape 2 : t : %v\n",t)
>	
>	Printf("p : %v *p: %v\n",p,*p)
> ```

## intervertir deux éléments d'un tableau

```go
t := []int{3,6,8,9,34,5}
	
t[4], t[2] = t[2], t[4]
	
Printf("t : %v\n",t)
```

```
t : [3 6 34 9 8 5]
```



