#! /bin/bash
echo "hello conditions ... "
echo "comparaison avec 3"

read -p "ecrire un nombre " nb
if [ $nb -eq 3 ]; then
    echo "égalité"
fi
if [ $nb -ne 3 ]; then
    echo "différent"
fi
if [ $nb -gt 3 ]; then
    echo "plus grand "
fi
if [ $nb -lt 3 ]; then
    echo "plus petit"
fi
if [ $nb -ge 3 ]; then
    echo "plus grand ou égal"
fi
if [ $nb -le 3 ]; then
    echo "plus petit ou égal"
fi