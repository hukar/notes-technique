#! /bin/bash

function init_variable() {
    echo "initialisation de la variable name"

    local name="michel"

    echo "utlisation local : $name"
    
}

init_variable
echo "utilisation en dehors de la fonction :$name"