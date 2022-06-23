# 06 La boucle `while`

```
while [ condition ]
do
	cmd
	cmd
	cmd
	...
done
```

Exemple :

```bash
#! /bin/bash

nb=$1

while [ $nb -gt 0 ]
do
    echo "nombre : $((nb--)) "
done
```

### Demander le prénom

```bash
#! /bin/bash

while [ -z $name ]
do
    read -p "quel votre prénom ? "  name
done

echo "vous vous appelez $name"
```

```
./while.sh
quel votre prénom ? 
quel votre prénom ? 
quel votre prénom ? 
quel votre prénom ? titi
vous vous appelez titi
```

Tant que le prénom n'est pas rempli, le script le redemande.

`[ -z $chaine ]`  vrai quand la chaine `$chaine`  est vide