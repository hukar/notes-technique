#! /bin/bash
min=11
max=49
result=999

nb=$(( (RANDOM % (max - min + 1) ) + min ))

echo "Trouvez un chiffre entre $min et $max"

while [ $result -ne $nb ]
do
    read -p "entrez un chiffre : " result
    if [ $result -lt $nb ]
    then
        echo "le chiffre est plus grand"
    elif [ $result -gt $nb ]
    then
        echo "le chiffre est plus petit"
    fi
done

echo "bravo vous avez gagné : $nb"