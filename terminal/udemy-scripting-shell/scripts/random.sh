#! /bin/bash

min=5

max=8

letter=""

while [ -z $letter ]
do
    echo $(( ($RANDOM % (max +1 - min) ) + min ))
    read -p "voulez-vous continuer ? " letter
done