#!/bin/bash
echo "Entrer un chiffre"
read CHIFFRE
while [ $CHIFFRE -gt 0 ]
do
CHIFFRE=$((CHIFFRE-1))
echo $CHIFFRE
done