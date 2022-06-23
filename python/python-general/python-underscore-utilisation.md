
# Python underscore utilisation

## private 

ajouter le `_` devant une méthode la rend privée pour les autres package


```python
from my_func import *
```


```python
with open("my_func.py","r") as f:
    print(f.read())
```

    def dodo():
        return 'dodo'
    
    def _secret():
        return 'secret'
    
    def reveal():
        return _secret()
    
    
    


```python
# from my_func import *

print(dodo())
```

    dodo
    


```python
print(_secret())
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    <ipython-input-4-d5b8a7445777> in <module>
    ----> 1 print(_secret())
    

    NameError: name '_secret' is not defined



```python
print(reveal())
```

    secret
    

## Mots Réservés


```python
dict_ = dict([("un",1),("deux",2)])
print(dict_)
```

    {'un': 1, 'deux': 2}
    

Le underscore après un nom évite de crée une erreur avec un mot réservé.

##  magic method `__method__`

### `__del__` 


```python
import time

class Action:
    
    name = "coco"
    
    def __del__(self):
        print(self.name,"c'est fini")
        
a = Action()

del a

time.sleep(3) # pour laisser le temps au garbage collector de passer

print("fini !")
```

    coco c'est fini
    fini !
    

### `__str__` 

La fonction toSrTring, offre une représentation de l'objet


```python
class Licorne:
    def __init__(self, name, color):
        self.name = name
        self.color = color
    
    def __str__(self):
        return f"\tname: {self.name}\t color: {self.color}\n"
    
l = Licorne("maaja", "green")
ll = Licorne("hült", "raimbow")

print l, ll 
```


      File "<ipython-input-25-e3fbb701d101>", line 12
        print l, ll
              ^
    SyntaxError: Missing parentheses in call to 'print'. Did you mean print(l, ll)?
    


### ``__repr__`` 

Donne la représention de l'objet lorsqu'il est contenu dans une collection et que print est appelé ou lorsqu'il est représenté dans un interpréteur.


```python
class Dragon:
    def __init__(self,name,color):
        self.name = name
        self.color = color
    def __str__(self):
        return f"__(Dragon {self.color}: {self.name})__"
    def __repr__(self):
        return f"<{self.name}, {self.color}>"

d = Dragon("tyranos", "gold")
l = [d,123,"go !!!"]
print(d)
print(l)
```

    __(Dragon gold: tyranos)__
    [<tyranos, gold>, 123, 'go !!!']
    


```python
class Data:
    def __str__(self):
        return 'str'
    def __repr__(self):
        return 'repr'
    def __format__(self, format):
        if format == 'my-custom-format':
            return "hey hello kitty!!"
        return "ola coco !"

d = Data()

print(f"{d!s} \t {d!r} \t {d:my-custom-format} \t {d}")
```

    str 	 repr 	 hey hello kitty!! 	 ola coco !
    
