#! /bin/bash

while [ -z $name ]
do
    read -p "quel votre prénom ? "  name
done

echo "vous vous appelez $name"