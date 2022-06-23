#! /bin/bash

function thimble() {
    nb=$(( (RANDOM % $1) + 1 ))

    return $nb
}

thimble $1

result=$?

echo $result