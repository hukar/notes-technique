# 10 Les codes erreur

De 0 à 255 (8 bits)

0 tout va bien 

1-255 erreur

## `$?`  code retour

exemple avec la commande `ping`  :

```bash
#! /bin/bash

hote=$1

nb_paquet=$2

ping -c  $nb_paquet $hote

if [ "$?" -ne "0" ]
then
    echo "L'hote $hote n'est pas joignable"
else
    echo "L'hote $hote est joignable"
fi
```

```bash
ping -c 1 8.8.8.8 # serveur de google
echo $?
```

```
0 # serveur bien contacté
```



## Sortir d'un script avec un code erreur personnalisé `exit 33#`

```bash
# fichier exit.sh
#! /bin/bash

echo "salut je vais sortir avec un code 33"

exit 33

echo "je suis encore là"
```

```bash
./exit.sh 
salut je vais sortir avec un code 33
echo $?
33
```

