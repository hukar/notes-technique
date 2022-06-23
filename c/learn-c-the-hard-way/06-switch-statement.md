# 06 switch statement

Le switch en C se comporte comme une *jump table*.

Le programme "saute" à la valeur concernée et exécute tout le code jusqu'au `break`

```c
#include <stdio.h>

int main(int argc, char *argv[]) {

    if(argc != 2) {
        printf("ERROR: you nedd one argument.\n");
		
        // par convention on retourne une valeur plus grande que 0
        return 1;
    }

    int i;
    char letter;
    for(i = 0, letter = argv[1][i]; argv[1][i] != '\0'; i++, letter = argv[1][i]) {

        switch(letter) {
            case 'a':
            case 'A':
                printf("%d :%c\n", i, letter - 32);
                break;
            case 'e':
            case 'E':
                printf("%d :%c\n", i, letter - 32);
                break;
            case 'i':
            case 'I':
                printf("%d :%c\n", i, letter - 32);
                break;
            case 'o':
            case 'O':
                printf("%d :%c\n", i, letter - 32);
                break;
            case 'u':
            case 'U':
                printf("%d :%c\n", i, letter - 32);
                break;
            case 'y':
            case 'Y':
                if(i > 2) {
                    printf("%d :%c\n", i, letter - 32);
                    break;
                }
                // break;
            default:
                printf("%d: %c is not a vowel\n", i, letter);
        }

    }

    return 0;
}
```

Les erreurs sont retournées avec un int plus grand que zéro.

`letter - 32` converti une minuscule en majuscule dans la table ASCII.

`'\0'` caractère de fin de chaîne. 