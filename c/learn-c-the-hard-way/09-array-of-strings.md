# 09 Array of strings in C

Il y a plusieurs façon de déclarer un string en C.

## `arrayStrings[][]`

Ce qui ne marche pas !

```c
char names[][] = {"robert", "rachel", "scott"};

for(int i = 0; i < 3; i++) {
    printf("name : %s\n", names[i]);
}
```

```bash
error: array has incomplete element type 'char []'
```

```c
char names[3][] = {"robert", "rachel", "scott"};

for(int i = 0; i < 3; i++) {
    printf("name : %s\n", names[i]);
}
```

```bash
error: array has incomplete element type 'char []'
```

Et maintenant ce qui marche :

```c
char names[3][12] = {"logan", "rachel", "scott"};

for(int i = 0; i < 3; i++) {
    printf("name : %s\n", names[i]);
    printf("taille de la chaine : %lu\n", sizeof(names[i]) / sizeof(char));
}
```

```bash
name : logan
taille de la chaine : 12
name : rachel
taille de la chaine : 12
name : scott
taille de la chaine : 12
```

Si on veut que la taille de chaque string soit implicite :

```c
char * names[3] = {"logan", "rachel", "scott"};

for(int i = 0; i < 3; i++) {
    printf("name : %s\n", names[i]);
}
```

```bash
name : logan
name : rachel
name : scott
```

### Différence de fragmentation mémoire pour les deux méthode :

Affichage `| indice : valeur ascii : caractère correspondant |`

A. `char array[NUMBER_OF_STRINGS][STRING_LENGTH]`

```c
char names[3][12] = {"a", "baba", "caravane"};

for(int i = 0; i < 40; i++) {
    printf("| %d : %d : %c ", i, names[0][i], names[0][i]);
}
```

```bash
| 0 : 97 : a | 1 : 0 :  | 2 : 0 :  | 3 : 0 :  | 4 : 0 :  | 5 : 0 :  | 6 : 0 :  | 7 : 0 :  | 8 : 0 :  | 9 : 0 :  | 10 : 0 :  | 11 : 0 :  | 12 : 98 : b | 13 : 97 : a | 14 : 98 : b | 15 : 97 : a | 16 : 0 :  | 17 : 0 :  | 18 : 0 :  | 19 : 0 :  | 20 : 0 :  | 21 : 0 :  | 22 : 0 :  | 23 : 0 :  | 24 : 99 : c | 25 : 97 : a | 26 : 114 : r | 27 : 97 : a | 28 : 118 : v | 29 : 97 : a | 30 : 110 : n | 31 : 101 : e | 32 : 0 :  | 33 : 0 :  | 34 : 0 :  | 35 : 0 :  | 36 : -2 : � | 37 : 127 :  | 38 : 0 :  | 39 : 0 :
```

####! le 37ème caractère est le numéro 127 dans la table `ASCII` est c'est `DELETE`.

####Ce qui crée cet affichage dans la console : `37 :|` car l'`espace` est `delete`.

On voit que l'espace mémoire est envahi de vide (0) = fragmentation.

Le point d'interrogation en 36 a la valeur -2, ce qui ne correspond à aucun caractère.

B. `char * array[NUMBER_OF_STRINGS]`

```c
char * names[3] = {"a", "baba", "caravane"};

for(int i = 0; i < 40; i++) {
    printf("| %d : %d : %c ", i, names[0][i], names[0][i]);
}
```

```bash
| 0 : 97 : a | 1 : 0 :  | 2 : 98 : b | 3 : 97 : a | 4 : 98 : b | 5 : 97 : a | 6 : 0 :  | 7 : 99 : c | 8 : 97 : a | 9 : 114 :r | 10 : 97 : a | 11 : 118 : v | 12 : 97 : a | 13 : 110 : n | 14 : 101 : e | 15 : 0 :  | 16 : 124 : | | 17 : 32 :   ...
```

Notre tableau de chaîne de caractère se termine en indice 15 contre l'indice 32 précédemment.

Un seul zéro sépare les différentes chaînes, il n'y a pas de fragmentation.



