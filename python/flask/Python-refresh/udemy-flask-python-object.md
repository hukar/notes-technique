
# Udemy Flask Python Object 

## Différence entre un dictionnaire et une classe 


```python
lottery_player_dict = {
    'name': 'rolf',
    'numbers': (5,6,3,45,1)
}

class LotteryPlayer:
    def __init__(self, name="rolf"):
        self.name = name,
        self.numbers = (5,6,3,45,1)
        
    def total(self):
        return sum(self.numbers)
        
player = LotteryPlayer()
print(player)
```

    <__main__.LotteryPlayer object at 0x000002424687BE80>
    

`__init__` est le constructeur il prend `self` la référence vers l'objet en premier paramètre.  
`name='rolf'` définie une valeur par défaut au paramètre `name`


```python
help(player)
```

    Help on LotteryPlayer in module __main__ object:
    
    class LotteryPlayer(builtins.object)
     |  Methods defined here:
     |  
     |  __init__(self)
     |      Initialize self.  See help(type(self)) for accurate signature.
     |  
     |  ----------------------------------------------------------------------
     |  Data descriptors defined here:
     |  
     |  __dict__
     |      dictionary for instance variables (if defined)
     |  
     |  __weakref__
     |      list of weak references to the object (if defined)
    
    


```python
print(player.__weakref__,player.__dict__)
```

    None {'name': ('rolf',), 'numbers': (5, 6, 3, 45, 1)}
    

Un dictionnaire ne peut pas intervenir sur ses propres datas


```python
player2 = lottery_player_dict
# help(player2)


print(player.name, player2["name"])
```

    ('rolf',) rolf
    


```python
# pour le dictionnaire
s = sum(player2["numbers"])
print(s)

# pour un objet
t = player.total()
print(t)
```

    60
    60
    


```python
p1 = LotteryPlayer() # va prendre la valeur par défaut
p2 = LotteryPlayer('tomy')
print(p1.name, p2.name)
```

    ('rolf',) ('tomy',)
    


```python
# changer la valeur d'un attribut
p2.name = 'mitchel'
print(p2.__dict__)
```

    {'name': 'mitchel', 'numbers': (5, 6, 3, 45, 1)}
    

## Transformer un objet en dictionnaire `__dict__` 


```python
d2 = p2.__dict__
print(d2, type(d2))
```

    {'name': 'mitchel', 'numbers': (5, 6, 3, 45, 1)} <class 'dict'>
    
