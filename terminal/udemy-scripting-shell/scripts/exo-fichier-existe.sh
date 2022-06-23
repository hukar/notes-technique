#! /bin/bash
function nb_argument() {
    if [ $1 -eq 0 ]
    then
        echo "Vous devez introduire des noms de fichier"
        exit 2
    fi
}

function fichier_exist() {
    ls $1 2> err.txt

    if [ $? -eq 0 ]
    then
        echo "le fichier $1 existe"
    else
        echo "le fichier $1 n'existe pas"
    fi
}

nb_argument $#

for f in $*
do
    fichier_exist $f
done