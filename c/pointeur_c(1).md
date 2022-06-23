# pointeur de fonction

```c
void attaquer (int* pvVie,char* typePerso) {
    if (*typePerso == 'm') {
        *pvVie -= 25;
    } else {
        *pvVie -= 23;
    }
    
    printf("point de vie(s) %d\n",*pvVie);

}



int main(int argc, const char * argv[]) {
    // pointeur sur fonction
    void(*att)(int*,char*);
    att = attaquer;
    int pv = 100;

    char type = 'f';

    att(&pv,&type);
}
```

On déclare le pointeur de fonction de cette manière
`void(*monPtr)(arg1,arg2*);`

On lui associe le nom d'une fonction exisatnte

`monPtr = nomFonctionExistante`