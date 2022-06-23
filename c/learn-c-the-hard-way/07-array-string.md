# 07 Array et String

Un string est un tableau de char en C.

## Initialisation

```c
int t0[4];
int t1[4] = {0};
int t2[4] = {1};
int t3[4] = {9, 8};
```

sortie :

```bash
[852534672 32764 0 0]
[0 0 0 0]
[1 0 0 0]
[9 8 0 0]
```

1. Déclaration d'un tableau sans initialisation : les valeurs affichées sont les déchéts de l'espace mémoire alloué.
2. On initialise la première valeur à zéro, les autres valeurs sont automatiquement initialisées à zéro.
3. La première valeur est initialisée à un.
4. La première valeur est initialisée à neuf et la deuxième à huit.

On peut initialiser avec toutes les valeurs dans ce cas pas besoin de spécifier la taille :

`showTab` :

```c
void showTab(int tab[], int length) {
    printf("[");

    for(int i = 0; i < (length - 1); i++) {
        printf("%d ", tab[i]);
    }

    printf("%d]\n", tab[length - 1]);
}
```

```c
char name[] = {'e','h','o','\0'};
int numbers[] = {1, 2, 3, 4};

printf("%s\n", name);

showTab(numbers, 4);
showTab(numbers, 8);
```

```batch
eho
[1 2 3 4]
[1 2 3 4 1487526208 32765 0 7301221]
```

Comme on ne spécifie pas de taille, aucunes valeurs n'est initialisée à 0, le tableau a la taille du littérale.

autre exemple :

```c
int num2[6] = {56, 47, 67, 89};

int numbers[] = {1, 2, 3, 4};

showTab(numbers, 4);
showTab(num2, 6);

showTab(numbers, 18);
```

```bash
[1 2 3 4]
[56 47 67 89 0 0]
[1 2 3 4 56 47 67 89 0 0 56 2054750208 4196048 0 -361970975 32583 262144 0]
```

1. Le tableau a une taille implicite de 4

2. Le tableau a explicitement une taille de 6, deux zéro sont ajoutés à l'initialisation.

3. On observe l'état de la pile :

   | variable                                 | valeur |
   | ---------------------------------------- | ------ |
   | numbers[0]                               | 1      |
   | numbers[1]                               | 2      |
   | numbers[2]                               | 3      |
   | numbers[3]                               | 4      |
   | num2[0]  (numbers[4])                    | 56     |
   | num2[1]  (numbers[5])                    | 47     |
   | num2[2]  (numbers[6])                    | 67     |
   | num2[3]  (numbers[7])                    | 89     |
   | num2[4]  (numbers[8]) initialisation à 0 | 0      |
   | num2[5]  (numbers[9]) initialisation à 0 | 0      |
   | reste de la pile  (numbers[10])          | 56     |


> ### Shortcut C : Si on initialise la première valeur, toutes les autres sont automatiquement initialisées à 0.

## Breaking

On ajoute un élément en plus de la taille du tableau :

```c
int t5[4] = {1, 2, 3, 4, 5};

showTab(t5, 4);
printf("t5[2] : %d\n", t5[2]);
printf("t5[3] : %d\n", t5[3]);
printf("t5[4] : %d\n", t5[4]);
```

sortie :

```bash
# affichage d'un warning
[1 2 3 4]
t5[2] : 3
t5[3] : 4
t5[4] : 9
```

## array char = string

En C une chaîne de caractère est un tableau de `char`.

```c
char s1[4];
char s2[4] = {'a'};
char s3[4] = {'a','b'};

printf("%c %c %c %c\n", s1[0], s1[1], s1[2], s1[3]);
printf("%d %d %d %d\n", s1[0], s1[1], s1[2], s1[3]);

printf("%c %c %c %c\n", s2[0], s2[1], s2[2], s2[3]);
printf("%d %d %d %d\n", s2[0], s2[1], s2[2], s2[3]);

printf("%c %c %c %c\n", s3[0], s3[1], s3[2], s3[3]);
printf("%d %d %d %d\n", s3[0], s3[1], s3[2], s3[3]);
```

sortie :

```bash
6 � �
54 -80 -79 2

a
97 0 0 0

a b
97 98 0 0
```

1. valeur aléatoire de la mémoire.
2. première valeur à `'a'` , toutes les autres à `'\0'`  
   équivalent à  `{'a'} <=> {'a', '\0', '\0', '\0'}`.
3. première valeur à `'a'`, deuxième valeur à `'b'`.

> ###char '\0' <=> int 0; char '0' <=> int 48 ASCII

## Initialisation valeur par valeur

On peut aussi initialiser chaque valeur indépendamment :

```c
char name[4];
    
name[1] = 'h';
name[2] = 'e';
name[3] = 'y';
name[4] = '\0';

printf("%c\n", name[1]);
```

```bash
h
```



### breaking

Différence de compilateur.

```c
char *name;
    
name[1] = 'h';
name[2] = 'e';
name[3] = 'y';
name[4] = '\0';

printf("%c\n", name[1]);
```

avec **gcc version 4.6.3** :

```bash
exit status -1
```

avec **clang Apple LLVM version 10.0.0 (clang-1000.11.45.5)** :

```bash
h
```

Même chose pour :

```c
 int *num;
    
num[1] = 1;
num[2] = 2;
num[3] = 3;
num[4] = 0;

printf("%d\n", num[1]);
```

```bash
Segmentation fault: 11
```



### erreur pour les deux compilateur

```c
int *num = {1, 2, 3, 0};

printf("%d\n", num[1]);
```

## String littéral

```c
char *name = "fred";
// char name[] = "fred"

printf("%c\n", name[2]);
```

```bash
e
```

L'afficher comme une chaîne :

```c
printf("%s\n", name);
```

```bash
fred
```

## Correspondance entre les int et les string

Un `int` vaut 4 bytes (4 octets), un mot de trois lettres aussi (1 `char` = 1 byte).

On peut donc obtenir facilement une correspondance entre un int et une chaîne de caractère de 3 lettres :

```c
char *name = "yop";
printf("yop : %s %d\n", name, name);

int a = 4196118;

printf("a : %d %s", a, a);
```

```bash
yop : yop 4196118
a : 4196118 yop
```

La suite binaire est identique, seul la manière de l'interpréter entre `%d` et %s change.

## Breaking

```c
void showString(char str[], int size) {
    printf("[");
    for(int i = 0; i < size - 1; i++) {
        printf("%c ", str[i]);
    }
    printf("%c]\n", str[size - 1]);
}
```

```c
char x = 'x';
char y = 'y';
char z = 'z';

char tab[] = {'a'};

tab[1] = 'b';

showString(tab, 4);

printf("z : %c", z);
```

```bash
[a b y x]
z : b
```

> ###En C il n'y a aucunes protection contre les dépassement d'indice d'un tableau, on peut modifier par inadvertance la valeur d'autres variables -sic-.

## Copier un string

Écriture utilisant le fait qu'une assignation est une expression et retourne la valeur de son assignation.

Aussi que `0` vaut `false` et `!=0` vaut `true`.

```c
void strc(char * str, char * dest) {
    while(*dest++ = *str++);
}
```

