# 05 breaking array

On peut facilement en C "casser" son programme.

première version :

```c
#include <stdio.h>

enum {BLUE, RED, YELLOW} colors;

int val() {
    return RED;
}

int main(void) {

    char before_initiale = 'B';
    char initiale = 'A';

    char name[] = "pol";


    printf("my name is %c. %s\n", initiale, name);
    printf("before initiale %c\n", before_initiale);
    return 0;
}
```

sortie: 

```bash
my name is A. pol
before initiale B
```

Maintenant ajoutons un int à un *string* ( `= char[]`)

```c
printf("my name is %c. %s\n", initiale, name + 1);
```

sortie :

```bash
my name is A. ol
before initiale B
```

En fait name est un pointeur sur array avec `+ 1`, il avance d'un index.



Allons en position 4 et 5 du tableau `name` :

```c
char name[] = "pol";

name[4] = 'Z';
name[5] = 'Y';
```

sortie :

```bash
my name is Z. ol
before initiale Y
```

On imagine la pile mémoire modifiée successivement :

| position dans la pile (stack) | départ | après modification                 |
| ----------------------------- | ------ | ---------------------------------- |
| name[0]                       | p      | p                                  |
| name[1] == name + 1           | o      | o (pointeur ici) name + 1 -> o     |
| name[2]                       | l      | l                                  |
| name[3]                       | '\0'   | '\0'  (caractère de fin de chaîne) |
| initiale                      | A      | Z (name[4])                        |
| before_initiale               | B      | Y (name[5])                        |





