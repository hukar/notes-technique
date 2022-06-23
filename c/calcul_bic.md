# calcul de bit

__&__ et binaire  
__|__ ou binaire  
__~__ inversion des bits  
__<<__ décalage à gauche  
__>>__ décalage à droite

## mettre un bit à 1

```c
  char a = 1;
  printf("a: %d\n",a);  // 0000 0001 = 1
  
  a |= (1<<3); // mettre à 1 le bit trois
  // 0000 0001
  //+0000 1000
  //-----------
  // 0000 1001 
  
  printf("a: %d\n",a);  // 0000 1001 = 9
  
  return 0;
```

## mettre à zéro un bit

```c
	char a = 9 // 0000 1001 = 9
  a &= ~(1<<3);
  //   0000 1001
  //(*~0000 1000) on inverse les bits
  // * 1111 0111
  // -------------
  //   0000 0001
  printf("a: %d\n",a);  // 0000 0001 = 1
```

## tester la valeur d'un bit
En c 0 vaut faux et n'importe quelle valeur vaut vrai  

```c
 char a = '5'; // 0000 0101
 char test;
 test = a & (1<<n); // onteste la place n+1
 
 test = a & (1<<2) 
 // a = 0000 0101
 //1<<2 0000 0100
 // &   0000 0100
 // test vaut 4 donc est vrai
 // le bit 3 est à un 1
 printf("test %d\n",test); vaut 0 si bit à 0 
```