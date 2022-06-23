# 17 Substitutuon de paramètre

## Valeur par défault `${ ...-... }`

### Si le paramètre n'existe pas

```bash
kms: ~ $ echo ${toto-titi}
titi
kms: ~ $ echo ${toto:-titi}
titi
```

### Si le paramètre existe mais est vide

`-` n'affiche rien

`:-` affiche la valeur par défaut

```bash
kms: ~ $ toto=

kms: ~ $ echo ${toto-titi}

kms: ~ $ echo ${toto:-titi}
titi
```

### Si le paramètre existe

```bash
kms: ~ $ toto=michel

kms: ~ $ echo ${toto:-titi}
michel
kms: ~ $ echo ${toto-titi}
michel
```

## Longueur d'une chaine `${#mavar}`

```bash
kms: ~ $ echo ${#toto}
6
```

## Supprimer une partie de la chaîne. 

## en commençant par le début  `#` ou `##`

### `#` la plus petite partie ou `##` la plus grande partie de la chaîne

```bash
kms: ~ $ url=http://coco.be/coco/open/coco/image.jpg

kms: ~ $ echo ${url#*coco}
.be/coco/open/coco/image.jpg

kms: ~ $ echo ${url##*coco}
/image.jpg

kms: ~ $ echo ${url#*/}
/coco.be/coco/open/coco/image.jpg

kms: ~ $ echo ${url##*/}
image.jpg
```

## en commençant par la fin `%` ou `%%`

Changer l'extension :

```bash
kms: ~ $ echo ${url%.jpg}.png
http://coco.be/coco/open/coco/image.png
```

```bash
kms: ~ $ echo ${url%coco*}
http://coco.be/coco/open/

kms: ~ $ echo ${url%%coco*}
http://
```

## Rechercher et remplacer `/` ou `//`

### Une seule occurence `/`

### `${string/search/replace}`

```bash
kms: ~ $ x="one cat walk in the street, this cat is very clever"

kms: ~ $ echo ${x/cat/dog}
one dog walk in the street, this cat is very clever
```

### Toutes les occurences `//`

### `${string//searchAll/replaceAll}`

```bash
kms: ~ $ echo ${x//cat/dog}
one dog walk in the street, this dog is very clever
```

## Récupérer une sous-chaîne

### `${string:offset[:length]}`

```bash
kms: ~ $ name="mister john"

kms: ~ $ echo ${name:7}
john

kms: ~ $ echo ${name:0}
mister john

kms: ~ $ echo ${name:-1}
mister john

kms: ~ $ echo ${name:1}
ister john
```

```bash
kms: ~ $ echo ${name:7:1}
j
```

