
# Listes, Tuples et Sets


```python
l = [1,"un"]
```


```python
t = (1, "deux")
```


```python
# syntaxe alternative
t2 = (5,) # pour un élément
print(t2)
t3 = 5,
print(t3)
```

    (5,)
    (5,)



```python
t4 = 1, 2, 3
print(t4)
```

    (1, 2, 3)


On peut donc écrire un tuple sans les parenthèses comme une suite de valeurs séparées par une virgule.


```python
s = {1,"deux"}
```


```python
grades = [12, 15, 6, 10, 8, 19]

def _average(grades):
    return sum(grades) / len(grades)

_average(grades)
```




    11.666666666666666




```python
gr_2 = (12, 15, 6, 10, 8, 19)
_average(gr_2)
```




    11.666666666666666




```python
gr_3 = {12, 15, 6, 10, 8, 19}
_average(gr_3)
```




    11.666666666666666



Python n'étant pas typé, on peut avoir des élément hétérogènes dans la même collection.


```python
patate = ("patate",8)

my_list = [patate, "patate", 45, True, 8.23]

my_list_type = [type(item) for item in my_list]
print(my_list_type)
```

    [<class 'tuple'>, <class 'str'>, <class 'int'>, <class 'bool'>, <class 'float'>]


il n'y a pas de possibilité de changer la taille d'un tuple.
Les tuples sont **immutable**.


```python
t.append(4)
```


    ---------------------------------------------------------------------------
    
    AttributeError                            Traceback (most recent call last)
    
    <ipython-input-18-ada7ed8a579e> in <module>
    ----> 1 t.append(4)


    AttributeError: 'tuple' object has no attribute 'append'



```python
t = (1,"deux",3,"quatre")
print(t)
```

    (1, 'deux', 3, 'quatre')

Mais **réassignable**.




```python
l.append(4)
```


```python
print(l)
```

    [1, 'un', 4]


Les set on des valeurs **unique** et **non-ordonnées**


```python
s = {1,4,1,4,1,4,4,4,1,5,3,3}
print(s)
```

    {1, 3, 4, 5}



```python
my_set_two = {"hello","kitty","how","are","you"}
print(my_set_two)
```

    {'kitty', 'how', 'are', 'hello', 'you'}

