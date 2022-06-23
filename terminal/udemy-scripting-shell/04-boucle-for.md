# 04 La boucle for

```bash
for i in obj1 obj2 obj3 # liste d'objet
do
	cmd
	cdm
done
```

Exemple :

```bash
for i in "titi" "toto" "tata"
do
	echo $i >> result.txt
done
```

## Une liste de valeurs dans une variable

```bash
variable="un deux trois"

for i in $variable
do 
	echo " $i ----- "
done
```

```
un --- 
deux --- 
trois --- 
```

## Utiliser le retour d'une commande

```bash
for i in `ls` # ou bien $(ls)
do 
    echo $i | grep i
done
```

```
condition.sh
empty-string.sh
equality-string.sh
variable.sh
```

