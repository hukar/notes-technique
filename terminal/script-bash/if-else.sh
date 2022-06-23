#! /bin/bash

read -p "how old are you" age

if [ $age -gt 100 ]; then
    echo "you are not very young"
else
    echo "you are still very young"
fi