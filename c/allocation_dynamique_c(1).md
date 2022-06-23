# Allocation dynamique en c

Inclure la bibliothèque \<stdlib.h\>

__malloc__ Memory ALLOCation

### dépassement de mémoire

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


typedef struct Personnage {
    int pv;
    char name[12];
    int tab[90000000000000];
    int tabi[90000000000000];
} Perso;

int main(int argc, const char * argv[]) {
    // insert code here...
    int *ptrInt;

    ptrInt = malloc(sizeof(Perso));
    
    if (ptrInt == NULL) {
        printf("dépassement de mémoire\n");
        exit(13);
    } else {
    *ptrInt = 7;
    }
    
    printf("valeur de ptrInt %d\n",*ptrInt);
    printf("Perso %ld\n",sizeof(Perso));
 
    return 0;
}
```

On obtient :

```c
alloc_dyna(5681,0x10011e3c0) malloc: 
*** mach_vm_map(size=720000000004096) failed (error code=3)
*** error: can't allocate region
*** set a breakpoint in malloc_error_break to debug
dépassement de mémoire
Program ended with exit code: 13
```

`exit()` pour sortir du programme

### allocation pour une structure

```c
typedef struct Personnage {
    int pv;
    char name[12];
    } Perso;
void init_jojo(Perso *ptr);

int main(int argc, const char * argv[]) {
    // insert code here...
    Perso *pJojo;
    
    pJojo = malloc(sizeof(Perso));
    init_jojo(pJojo);
    
    printf("nom du perso %s\n",pJojo->name);
 
    return 0;
}

void init_jojo (Perso *ptr) {
    strcpy(ptr->name,"jojo");
    ptr->pv = 56;
}
```

On initialise bien un pointeur sur structure

### tableau dynamique

```c
int *amis,nbAmis,i;
    
printf("nombre d'amis?\n");
scanf("%d",&nbAmis);
    
amis = malloc(nbAmis*sizeof(int));
    
for (i = 0;i < nbAmis;printf("quel age a votre amis %d\n",i),scanf("%d",&amis[i]),i++);
    
for (i = 0;i < nbAmis;printf("age amis : %d\n",amis[i]),i++);

free(amis);
```

Attention c'est `&amis[i]` dans le scanf

autre écriture de for :

```c
for("initalisation de l'itérateur";"test";"instruction1,instruction2,...,incrémentation de l'itérateur");
```

Avec point virgule à la fin
`free()` à ne pas oublier à la fin.

### avec calloc

```c
int tailleTab,cpt,*tab = NULL;
do {
printf("veuillaez introduire la taille du tableau :\n");
scanf("%d",&tailleTab);
    
tab = calloc(tailleTab,sizeof(int));
    
if (tab == NULL) exit(9);
    
for (cpt = 0;cpt < tailleTab;tab[cpt]=cpt*3,cpt++);
for (cpt = 0;cpt < tailleTab;cpt++)
    if (cpt > tailleTab -5) printf("[%d]\n",tab[cpt]);
} while (tailleTab != 0);
    
free(tab);
```

calloc(nbElt,tailleElt) nombre déléments et taille d'éléments avec sizeof

Ne pas oublié de tester l'allocation avec if(tab == NULL) exit(9)

Ne pas oublier de libérer la mémoire avec free(tab)

### realloc

```c
tab = calloc(tailleTab,sizeof(int));
    
if (tab == NULL) exit(9);
    
   
for (cpt = 0;cpt < tailleTab;cpt++) {
    tab[cpt] = ((cpt + 2)*cpt)/3;
    printf("[%d]\n",tab[cpt]);
}
    
tailleTab = 20;
    
tab = realloc(tab,tailleTab*sizeof(int));
```

realloc permet de re-allouer de la mémoire en cas de changement de taille