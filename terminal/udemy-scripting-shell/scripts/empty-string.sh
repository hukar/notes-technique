#! /bin/bash

read -p "Introduisez votre nom : " name
for i in {1..999}
do
    if [ -z $name ]
    then
        read -p "veuillez entrer votre nom deux fois !" name
    else
        echo "merci $name"
        break
    fi
done

echo "fin du programme"
