#! /bin/bash

echo "hello I am a script"
read -p "What's your name " username

echo "hello $username nice to meet you"
read -p "tell me the path of the file you want to open " filepath

open $filepath
echo "bye bye ---_<(<)/°= "
