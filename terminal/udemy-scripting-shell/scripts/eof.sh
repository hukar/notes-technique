#! /bin/bash

read -p "entrez une voyelle " voy

grep $voy << EOF > eof.txt
> ici une ligne
> une seconde ligne
> une troisième ligne avec a
> EOF