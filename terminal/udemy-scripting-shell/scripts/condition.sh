#! /bin/bash

read -p "ecrire un nombre " nb

if [ $nb -eq 99 ]
then
    echo "bravo !!"
elif [ $nb -lt 99 ]
then
    echo "trop petit"
elif [ $nb -gt 99 -a $nb -le 1000 ]
then
    echo "trop grand"
else
    echo "beaucoup trop grand"
fi