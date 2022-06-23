#! /bin/bash

echo "le nombre de paramètres \$# : $#"
echo "la liste des paramètres \$* : $*"

echo "premier argument : $1"
shift
echo "deuxième argument : $1"