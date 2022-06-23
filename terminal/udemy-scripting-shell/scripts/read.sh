#! /bin/bash
case "$1" in
    titi)
        read -p "oui opu non (y/n) " mot
        echo $mot

        case "$mot" in
            y)
                echo "yes tata"
                ;;
            n)
                echo "no tata"
                ;;
            *)
                echo "star two"
                ;;
        esac
        ;;
    toto)
        echo "toto"
        ;;
    *)
        echo "star"
        ;;
esac