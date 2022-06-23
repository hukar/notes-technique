# 03 Formated print

Voici le prototype de `printf` :

```c
int printf(const char* format, ...);
```

`printf` est une fonction variadique `...` , c'est à dire qu'elle peut prendre un nombre variable de paramètre. 

Pour afficher quelque chose on utilise :

```c
int age = 45;

printf("mon age est de %d années\n",age);
```

### Les formats

| type      | lettre |
| --------- | ------ |
| Integer   | %d     |
| Long      | %ld    |
| Character | %c     |
| String    | %s     |
| Pointer   | %p     |



## Debugger lldb

`lldb` + `enter`

ensuite on tape `run`

`ctrl + d` pour sortir