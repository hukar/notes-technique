#! /bin/bash

case "$1" in
    start)
        echo "le serveur est lancé"
        ;;
    stop)
        echo "le serveur est arrêté"
        ;;
    *)
        echo "hey caramba!"
        ;;
esac