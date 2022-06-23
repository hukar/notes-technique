#! /bin/bash

fichier=$1

cmd=$2

if [ -z $fichier ] || [ -z $cmd ]
then
    echo "nombre d'argument incorrecte"
    exit 1
fi

if [ -f $fichier ]
then
    case "$cmd" in
        copy)
            if [ ! -e tmp/copie ]
            then
                echo "Création du dossier /tmp/copie"
                mkdir -p tmp/copie
            fi
            echo "copie du fichier $fichier dans tmp/copie"
            cp $fichier tmp/copie
            ;;
        delete)
            read -p "êtes-vous sûre ? (y/n) " response

            case "$response" in
                y)
                    rm $fichier
                    echo "fichier $fichier supprimé"
                    ;;
                n)
                    echo "fichier $fichier non supprimé"
                    ;;
                *)
                    echo "mauvaise instruction"
                    ;;
            esac
            ;;
        read)
            cat $fichier
            ;;
        *)
            echo "commande non disponible"
            ;;
    esac
else
    echo "$fichier n'est pas un fichier valide"
fi

echo "fin du programme"