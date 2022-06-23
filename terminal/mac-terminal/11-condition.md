# 11 `if`

## Base `if [ condition ]; then … fi`

```bash
if [ 3 -eq 3 ]; then # ! attention aux espaces !
    echo "YES there the same"
fi
```

### Opérateurs logiques

* `-eq`  égal
* `-ne`  pas égal
* `-gt`  plus grand
* `-lt`  plus petit
* `-ge`  plus grand ou égal
* `-le`  plus petit ou égal
* `-a`  ET
* `-o`  OU

## `elif`  et `else`

#### ! Ne pas oublier  `;then`  après la condition

```bash
read -p "donner votre âge " age # -p ajoute une phrase

if [ $age -lt 0 ];then
    echo "vous n'êtes pas encore né"
elif [ $age -ge 0 -a $age -lt 18 ]; then # -a AND
    echo "vous êtes né mais pas majeur"
elif [ $age -ge 18 -a $age -lt 150 ]; then
    echo "vous êtes un majeur de $age ans"
else
    echo "menteur !!"
fi
```

## Comparaison de chaînes de carctère

Pour comparer deux chaînes de caractère, on utilise `==`  ou `!=` .

```bash
#! /bin/bash

read -sp "votre mot secret : " secret

if [ $secret == "poney" ];then
    echo "vous aimez les poneys"
elif [ $secret == "tortue" ];then
    echo "yahahah !!"
else
    echo "ALERT ALERTE !"
fi
```

