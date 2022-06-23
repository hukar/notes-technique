#! /bin/bash

for i in "$@"
do
    echo "@ $i"
done

for i in "$*"
do
    echo "* $i"
done

for i in $@
do
    echo "@2 $i"
done

for i in $*
do
    echo "*2 $i"
done

main() {
    echo "Main sees" $# " args"
}

main $*
main $@
main "$*"
main "$@"