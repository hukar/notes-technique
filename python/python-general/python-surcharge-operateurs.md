
# Surcharge des opérateurs 

## add | + operator


```python
class Legume:
    def __init__(self,name):
        self.name = name
        
    def __add__(self, other):
        return f"{self.name} plus {other.name}"
        
tomate = Legume("tomate")
concombre = Legume("concombre")

print(tomate + concombre)
```

    tomate plus concombre
    

## lshift | << operator 


```python
class Goblin:
    def __init__(self, name, age):
        self.name = name
        self.age = age
        
    def __str__(self):
        return f"{self.name} \t age:{self.age}"
    
    def __lshift__(self, value):
        self.age = self.age - value
    
g = Goblin("yul",25)
g << 3
print(g)
```

    yul 	 age:22
    

## Rendre un objet _callable_ comme une fonction 


```python
class Mecha:
    def __call__(self, tech):
        print("mecha mecha adore", tech)

m = Mecha()
m("informatique")
m("cuisine")
```

    mecha mecha adore informatique
    mecha mecha adore cuisine
    
