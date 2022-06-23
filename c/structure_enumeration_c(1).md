# structure et énumération

```c
typedef struct Personnage {
    int pv;
    int pm;
    char name[34];
    char typeClass[34];
    
} Personnage;

void prog(void);
void init_perso(Personnage *prs);

int main(int argc, const char * argv[]) {
    // insert code here...
    prog();
    return 0;
}

void prog () {
    Personnage jiji;
    init_perso(&jiji);
    printf("pv %d pm %d nom %s classe %s \n",jiji.pv,jiji.pm,jiji.name,jiji.typeClass);
}

void init_perso (Personnage *jojo) {
    (*jojo).pv = 20;
    (*jojo).pm = 13;
    strcpy(jojo->name,"quidam");
    strcpy(jojo->typeClass,"guerrier");
}
```

**typedef** permet de créer un alias de struct Personnage

__(*jojo).pv__ et __jojo->pv__ sont identiques

__strcpy__ permet de remplire un champ _string_ 

