# les tableau en c

## les tableaux de taille statique

### initialisation
```c
	int tab[5];
	tab[0] = 1;
	tab[1] = 2;
	etc ...
	
 	int tab[5];// = {22,23,24,25,26};  // initialisation
 	int tab[5] = {2} // donnera {2,0,0,0,0,0} 
 	// c remplace par 0 les valeurs non mentinnée
    
    // tableau a deux dimensions
    int damier[3][5] = {{1,2,3,4,5},
        {1,2,3,4,5},
        {1,2,3,4,5}};
```

### lire un tableau

```c
// 10 étant la taille du tableau
 for(i =0;i <10;i++){
        printf("valeur %d\n",tab[i]);
        printf("adresse %p\n",&tab[i]);
    }
```

### passer un tableau dans une fonction

```c
void incTab(int tab[],int taille){
    int i;
    
    for (i = 0;i < taille;i++){
        tab[i] += 1;
    }
}

// *tab = tab[] écriture au choix 
// seulement *tab ne dit pas que tab est un tableau
void affTab(int *tab,int taille){
    int i;
    
    for (i = 0;i < taille;i++){
        printf("tab :%d ",tab[i]);
    }
    printf("\n-----------------\n");
}


    int tab[5]  = {22,23,24,25,26};  // initialisation
    affTab(tab,5);
    > tab :22 tab :23 tab :24 tab :25 tab :26 
	> -----------------
    incTab(tab,5);
    
    affTab(tab,5);
    > tab :23 tab :24 tab :25 tab :26 tab :27 
	> -----------------
```

Les notations tab[] et *tab sont équivalentes
Il faut passer la taille du tableau pour pouvoir itérer sur celui-ci