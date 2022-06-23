# cours c isib

## installation

minGw installe les compilateurs et débuggueurs

on indique le chemin au système pour les utiliser

Dans l'invite de commande

`setx PATH "%PATH%;C:\MinGW\bin\"`

ou tout simplement dans les options avancés de système variable système

## variable

\_\_LINE__
\_\_TIME__
\_\_DATE__
\_\_FILE__

variables sytème utiles pour le debug

## pause sur windows 

évite que l'invite de commande se referme trop rapidement

`system("PAUSE");`
C'est une commande sytème on peut écrire :
`system("clear")`sous linux par exemple

## macros 

_WIN32

_WIN64

\_\_APPLE__

\_\_APPLE_CC__    version compilateur

## éviter l'inclusion infini

```c

#ifndef library_h // si pas défini ifndef
#define library_h


void blabla(char[]);

#endif 
```
`ifdef` si défini

## les pointeurs

```c
void attaquer(int *pv,int *pm,int att);
int main(int argc, const char * argv[]) {
       int pv = 100;
    int pm = 120;
    
    int *ptpv = &pv;
    int *ptpm = &pm;
    
    printf("valeur pv %d\n",pv);
    printf("valeur pm %d\n",pm);
    printf("adresse pv %p\n",&pv);
    
    printf("valeur pointé par ptpv %d\n",*ptpv);
    printf("adresse pointé par ptpv %p\n",ptpv);
    printf("adresse de ptpv %p\n",&ptpv);
    attaquer(ptpv,ptpm,40);
    
    printf("valeur pv %d\n",pv);  // donne 60
    printf("valeur pm %d\n",pm);  // donne 100
}

void attaquer(int *pv,int *pm,int att){
    *pv -= att;
    *pm -= att/2;
}
```