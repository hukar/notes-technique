#! /bin/bash

function internet() {
    ping -c $1 $2

    if [ $? -eq 0 ]
    then
        echo "la connexion fonctionne"
    else
        echo "pas de connection"
    fi
}

internet $1 $2