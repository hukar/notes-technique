#! /bin/bash

for i in `ls`
do
    echo "$i"

    if [ -f $i ]
    then
        echo "c'est un fichier"
    fi

    if [ -d $i ]
    then
        echo "c'est un dossier"
    fi
done