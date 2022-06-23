#! /bin/bash

echo "l'argument1 a pour valeur $1"

if [ -z $1 ]
then
    echo "Vous n'avez pas entré d'argument"
    echo "pour éxécuter le script"
fi