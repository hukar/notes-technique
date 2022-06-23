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