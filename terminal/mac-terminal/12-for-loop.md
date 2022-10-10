# 12 Boucle `for`

> ## Sauter une ligne dans le terminal `shift + enter`

```bash
for i in { ... }
do
	# something ...
done
```

Un exemple :

```bash
#! /bin/bash
here=$(pwd)

echo "Welcome to loop in $here"

for i in {6,89,5,12}
do 
    echo "i = $i"
done

echo "bye bye bye"
```

```
Welcome to loop in /Users/kms/Google Drive/note-technique/mac-terminal/script-bash
i = 6
i = 89
i = 5
i = 12
bye bye bye
```

## Définir un interval `{1..7}`

Les valeurs sont incluses :

```bash
#! /bin/bash

for i in {-4..2}
do
    echo "i : $i"
done
```

```
hello nombre : -4
hello nombre : -3
hello nombre : -2
hello nombre : -1
hello nombre : 0
hello nombre : 1
hello nombre : 2
```

## Boucler sur des valeurs de différents types

```bash
for i in {-4,"hello",33,"coco"}
do
    echo "hello nombre : $i"
done
```

```
i : -4
i : hello
i : 33
i : coco
```

## Break

#### ! pas d'espace dans une liste de valeurs

```bash
for i in {"blue","elephant","noodle","rice","danger","radio","robot"} # pas d'espace ici
do
    if [ $i == "danger" ];then
        echo "danger instruction"
        break
    fi
    echo "done : $i"
done
```

```
done : blue
done : elephant
done : noodle
done : rice
danger instruction
```



## Syntaxe sans accolade `{ ... }`

On peut juste mettre les éléments les un à la suite des autres avec un espace :

```bash
for fruit in coco banana strawberry
do
	echo "fruit : $fruit"
done
```

