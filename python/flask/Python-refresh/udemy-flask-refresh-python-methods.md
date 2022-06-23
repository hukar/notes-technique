
# Udemy flask 0 refresh python 1

test des magic IPython : %load


```python
with open("func.py","w") as f:
    f.write("print('hello coco')")
```


```python
# %load func.py
print('hello coco')
```

    hello coco


Avant l'éxécution on a %load et ensuite cela charge le contenu du fichier, puis on l'éxécute de nouveau

## méthode et arguments

En python on parle de méthodes et d'argument, leurs nopms se compose de lettre, de tirets bas et de chiffre (mais pas au début


```python
a = 'a'
a = "a"
```


```python
2a = "aa"
```


      File "<ipython-input-9-d8097296a2df>", line 1
        2a = "aa"
         ^
    SyntaxError: invalid syntax




```python
$a = "a"
```


      File "<ipython-input-10-5b437464105c>", line 1
        $a = "a"
        ^
    SyntaxError: invalid syntax




```python
a_a = "a_a"
```

### exemple de méthode


```python
def _private(secret):
    print(secret)
    
_private("mon secret")
```

    mon secret



```python
_private()
```


    ---------------------------------------------------------------------------
    
    TypeError                                 Traceback (most recent call last)
    
    <ipython-input-13-95ebd730a976> in <module>
    ----> 1 _private()


    TypeError: _private() missing 1 required positional argument: 'secret'

## Composition de fonction

```python
def one(param):
    def two(a):
        print(a, param)
    return two

oneSalutation = one("salutation")
oneSalutation("Bolos")
```

    Bolos salutation


On peut composer les fonctions entres elles
