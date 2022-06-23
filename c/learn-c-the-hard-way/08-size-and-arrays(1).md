# 08 size and arrays

`sizeof` renvoie le nombre de byte alloué en mémoire.

`__SIZEOF_INT__` constante de la taille d'un `int` en mémoire en bytes

```c
int tab1[6] = {5, 6, 7};

int memorySize = sizeof(tab1);
int intSize = __SIZEOF_INT__;

int tab1Size = memorySize / intSize;

printf("memory size : %d\n", memorySize); 
printf("int size : %d\n", intSize); 
printf("tab1 size : %d\n", tab1Size); 
```

```bash
memory size : 24
int size : 4
tab1 size : 6
```

Maintenant la même chose avec un string :

```c
char name[] = "hello kitty";

char a = 'a'; // uniquement pour récupérer la taille d'un char

memorySize = sizeof(name);
int charSize = sizeof(a);  // on sait que c'est un byte
int nameSize = memorySize / charSize;

printf("memory size : %d\n", memorySize); 
printf("char size : %d\n", charSize); 
printf("name size : %d\n", nameSize); 
```

```bash
memory size : 12
char size : 1
name size : 12
```

onze caractères plus le `'\0'` final.

## Breaking

maintenant si j'utilise l'écriture avec pointeur :

```c
char *name = "hello kitty";

// ...

printf("memory size : %d\n", memorySize); 
printf("char size : %d\n", charSize); 
printf("name size : %d\n", nameSize); 
```

```bash
memory size : 8
char size : 1
name size : 8
```

de même avec :

```c
char *name = "";
```

```bash
memory size : 8
char size : 1
name size : 8
```

L'explication est que `sizeof` renvoie la taille d'un pointeur et non pas celle du `string` !

```c
void * ptr;

printf("%lu\n", sizeof(ptr));
```

```bash
8
```

`%lu` = long unsigned

## Breaking

Si on oublie le caractère de fin de string `'\0'` , `printf("%s", myString)` continue d'afficher tous les caractère jusqu'à trouver un '\0' (0000 0000) :

```c
char tab[] = "hello les gars";

char name[] = {'t', 'o', 'm','\0'};

printf("name : %s\n", name);
```

```bash
name : tom
```

Maintenant *oublions* le `'\0'` :

```c
char tab[] = "hello les gars";

char name[] = {'t', 'o', 'm'};

printf("name : %s\n", name);
```

```bash
name : tomhello les gars
```

On peut corriger cela en indiquant la bonne taille du tableau :

```c

```

