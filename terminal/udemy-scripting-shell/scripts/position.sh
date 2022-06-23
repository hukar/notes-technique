#! /bin/bash
echo "position des paramètres"
for i in {0..9} 
do
     output="\$$i : "
     eval output+=\$$i
     echo $output
done