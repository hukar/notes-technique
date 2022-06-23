#! /bin/bash

read -p "dites un chiffre entre 0 et 9 " chiffre;

if [ $(( chiffre%2 )) -eq 0 ]; then
    echo "votre chiffre est paire"
else
    echo "votre chiffre est impaire"
fi