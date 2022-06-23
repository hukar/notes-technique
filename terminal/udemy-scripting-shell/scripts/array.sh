#! /bin/bash

echo "entrez dans le tableau"
read -a mon_tableau

for i in {0..2}
do
    echo ${mon_tableau[$i]}
done

echo ${mon_tableau[@]}
echo ${mon_tableau[*]}