#! /bin/bash

case "$1" in 
    [lL][uU][nN][dD][iI])
        echo "lundi"
        ;;
    [mM][aA][rR][dD][iI])
        echo "mardi"
        ;;
    *)
        echo "dimanche"
        ;;
esac