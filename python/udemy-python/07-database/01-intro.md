# 01 Database en Python

## Index dans une boucle

### enumerate

```python
list = ["un", "deux"]

for index, value in enumerate(list):
    print(f"{index} - {value}")
```

```bash
0 - un
1 - deux
```

## Switch/case en Python

Il faut utiliser un dictionnaire :

```python
def add_func(name):
    print("add function", name)

def delete_func(name):
    print("delete function", name)

result = input("choose a or d")

{
    'a': add_func,
    'd': delete_func,
}[result]("coco")
```

## Python conseil de style

Il faut laisser 2 lignes blanches entre deux fonctions.

## supprimer un élément d'une liste

### 1. avec une boucle

````python
l = ["hello","coco","titi","toto"]

def del_word1(w):
    for word in l:
        if word==w:
            l.remove(word)
            break
````

### 2. avec une liste de comprehension

De préférence pour ne pas manipuler une liste dans une boucle qui la lit.

````python
def del_word2(w):
    # l in the local scope is equal to l in the global scope
    global l
    l = [item for item in l if item != w]
````

mot clé `global` : appele la variable globale dans le scope local de la fonction.

## Les ternaires

```python
read = 'YES' if book['read'] else 'NO'
```

var = A si condition est vrai sinon B