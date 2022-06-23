# les chaînes de caractère

Toujours un `getchar()` après un scanf :

```c
 while (letter != 's')
    {
    printf("entrez une lettre :\n");
    scanf("%c",&letter);
    getchar();
    printf("votre lettre %c et son code ascii %d\n",letter,letter);
    }
```

celà vide le buffer

```c
signed char letters[7] = {'y','i','p','p','i','i'};
```

Il faut compter un caractère de fin en plus (6 lettres + \0)

### taille de la chaîne de caractère

```c
    char letters[7] = {'y','i','p','p','i'};
    printf("dites salut : %s\n",letters);
    printf("taille de la chaine %ld",strlen(letters));
    > taille de la chaine 5
```

%ld pour un long et %d pour un int

### tester une valeur de chaîne

```c
while (strcmp(letters,"stop")!=0)
    {
        scanf("%s",letters);
        printf("%s\n",letters);
    }
```

`strcmp` retourne zéro si ses deux arguments sont les mêmes chaînes

version avec l'ordre alphabétique

```c
 while(1)
    {
        int test;
        
        scanf("%s",letters);
        printf("%s\n",letters);
        
        test = strcmp(letters,"stop");
        
        if (test > 0) {
            printf("%d mot après stop\n",test);
        } else if (test < 0) {
            printf("%d mot avant stop\n",test);
        } else {
            printf("%d mot égal stop\n",test);
            break;
        }
    }
```

### copie d'une chaîne dans une autre

```c
 char mot1[26] = "yoyo";
char mot2[26];
    
strcpy(mot2,mot1);
printf("%s\n",mot2);
> yoyo
```

### concaténation

```c
    char mot1[26] = "yoyo";
    char mot2[26] = " rapido :) !";
    
    printf("%s\n",strcat(mot1,mot2));
    > yoyo rapido :) ! 
```

### recherche d'une chaîne dans une chaîne

```c
 char mot1[26] = "yo yo";
    char mot2[26] = "yoyo rapido :) !";
    
    
    printf("%s\n",strstr(mot2,mot1));
    > (null)
    
     char mot1[26] = "yoyo";
    char mot2[26] = "yoyo rapido :) !";
    
    
    printf("%s\n",strstr(mot2,mot1));
    > yoyo rapido :) !
```

renvoie null ou la chaîne elle-même

### recherche d'un caractère dans une chaîne

```c
    char mot2[26] = "yoyo rapido :) !";
    
    printf("%s\n",strchr(mot2,'e'));
    > (null)
    
    char mot2[26] = "yoyo rapido :) !";
    
    printf("%s\n",strchr(mot2,'y'));
    > yoyo rapido :) !
```

idem mais avec un caractère