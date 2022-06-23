#! /bin/bash

for i in ./
do
    echo $i
    
    if [ $i == "hello.txt" ];then
        echo "txt finded"
        "salut txt" >> $i
    fi
done