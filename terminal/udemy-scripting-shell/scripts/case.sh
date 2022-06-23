#! /bin/bash

read -p "quel est votre age ? " age

if [ $age -lt 18 ]
then
    echo "vous êtes mineur"
elif [ $age -ge 18 -a $age -lt 67 ]
then
    echo "vous êtes un majeur actif"
elif [ $age -ge 67 -a $age -lt 150 ]
then
    echo "vous êtes un senior"
else
    echo "vous êtes très très vieux"
fi

