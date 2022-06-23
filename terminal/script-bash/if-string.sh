#! /bin/bash

read -sp "votre mot secret : " secret

if [ $secret == "poney" ];then
    echo "vous aimez les poneys"
elif [ $secret == "tortue" ];then
    echo "yahahah !!"
else
    echo "ALERT ALERTE !"
fi