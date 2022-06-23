#! /bin/bash

echo "Bonjour je s'appelle Groot"
echo "Formation Bash"

while [ -z $name ]
do
    read -p "comment tu t'appelles ? " name
done

while [ -z $age ]
do
    read -p "quel est ton age ? " age
done

