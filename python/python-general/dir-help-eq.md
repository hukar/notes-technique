# Fonction de Python utiles

## dir(obj)

affiche la liste des attributs et des méthodes de l'objet.

## help(obj)

Affiche la documentation de l'objet.

## `is` et `==` avec `__eq__`

`is` renvoie true si ce sont les mêmes objets (compare les références)

`==` est déterminé par la méthode `__eq__(self, other)`

```python
a = [5]
b = [5]
print(a is b) # false compare la référence
print(a == b) # True
print('-------------')

class Point():
    num = 8
    # surcharge de l'opérateur ==
    def __eq__(self, other):
        if self.num > other:
            return True
    
        return False

c = Point()
print(c == 8) # False
print(c == 7) # True
```

## Introspection

```python
import inspect
from pprint import pprint

class Robot:
    name = ''
    age = ''

    def __init__(self, name, age):
        self.name = name
        self.age = age

    def hello(self):
        print(f"I'm {self.name} hello !")

r = Robot('Toni', 456)

print(r) # la référence

pprint(dir(r)) # le nom des membres
pprint(vars(r)) # juste les clés valeurs (pas les méthodes)
pprint(r.__dict__) # idem
pprint(inspect.getmembers(r)) # tout en détails sur un niveau
```

`pprint` est une version formatée de `print`

Resultat :

```bash
# print(r)
<__main__.Robot object at 0x10dc41190>

# pprint(dir(r))
['__class__',
 '__delattr__',
 '__dict__',
 '__dir__',
 '__doc__',
 '__eq__',
 '__format__',
 '__ge__',
 '__getattribute__',
 '__gt__',
 '__hash__',
 '__init__',
 '__init_subclass__',
 '__le__',
 '__lt__',
 '__module__',
 '__ne__',
 '__new__',
 '__reduce__',
 '__reduce_ex__',
 '__repr__',
 '__setattr__',
 '__sizeof__',
 '__str__',
 '__subclasshook__',
 '__weakref__',
 'age',
 'hello',
 'name']
 
 # pprint(vars(r))
{'age': 456, 'name': 'Toni'}

# ou pprint(r.__dict__)
{'age': 456, 'name': 'Toni'}

# pprint(inspect.getmembers(r))
[('__class__', <class '__main__.Robot'>),
 ('__delattr__', <method-wrapper '__delattr__' of Robot object at 0x10dc41190>),
 ('__dict__', {'age': 456, 'name': 'Toni'}),
 ('__dir__', <built-in method __dir__ of Robot object at 0x10dc41190>),
 ('__doc__', None),
 ('__eq__', <method-wrapper '__eq__' of Robot object at 0x10dc41190>),
 ('__format__', <built-in method __format__ of Robot object at 0x10dc41190>),
 ('__ge__', <method-wrapper '__ge__' of Robot object at 0x10dc41190>),
 ('__getattribute__',
  <method-wrapper '__getattribute__' of Robot object at 0x10dc41190>),
 ('__gt__', <method-wrapper '__gt__' of Robot object at 0x10dc41190>),
 ('__hash__', <method-wrapper '__hash__' of Robot object at 0x10dc41190>),
 ('__init__',
  <bound method Robot.__init__ of <__main__.Robot object at 0x10dc41190>>),
 ('__init_subclass__',
  <built-in method __init_subclass__ of type object at 0x7f9aaf7134b0>),
 ('__le__', <method-wrapper '__le__' of Robot object at 0x10dc41190>),
 ('__lt__', <method-wrapper '__lt__' of Robot object at 0x10dc41190>),
 ('__module__', '__main__'),
 ('__ne__', <method-wrapper '__ne__' of Robot object at 0x10dc41190>),
 ('__new__', <built-in method __new__ of type object at 0x10d97c9b0>),
 ('__reduce__', <built-in method __reduce__ of Robot object at 0x10dc41190>),
 ('__reduce_ex__',
  <built-in method __reduce_ex__ of Robot object at 0x10dc41190>),
 ('__repr__', <method-wrapper '__repr__' of Robot object at 0x10dc41190>),
 ('__setattr__', <method-wrapper '__setattr__' of Robot object at 0x10dc41190>),
 ('__sizeof__', <built-in method __sizeof__ of Robot object at 0x10dc41190>),
 ('__str__', <method-wrapper '__str__' of Robot object at 0x10dc41190>),
 ('__subclasshook__',
  <built-in method __subclasshook__ of type object at 0x7f9aaf7134b0>),
 ('__weakref__', None),
 ('age', 456),
 ('hello',
  <bound method Robot.hello of <__main__.Robot object at 0x10dc41190>>),
 ('name', 'Toni')]
```

