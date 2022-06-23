#! /bin/bash

read -p "veuillez entrer un nombre : " nb

while [ $nb -ne 0 ]
do
    echo "le compteur est à : $nb"
    nb=`expr $nb - 1`
done