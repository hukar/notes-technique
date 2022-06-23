# Udemy Flask refresh list tuple set 2

## Ajouter un élément


```python
l = [77, 80, 90]
t = (77, 80, 90)
s = {77, 80, 90}
```

## `append` pour les listes

```python
l.append(100)
print(l)
```

    [77, 80, 90, 100]

## Les tuples sont immutables

```python
# t immutable
```
## `add` pour les set
```python
s.add(100)
s.add(100)
print(s)
```

    {80, 90, 100, 77}



```python
help(s)
```

    Help on set object:
    
    class set(object)
     |  set() -> new empty set object
     |  set(iterable) -> new set object
     |  
     |  Build an unordered collection of unique elements.
     |  
     |  Methods defined here:
     |  
     |  __and__(self, value, /)
     |      Return self&value.
     |  
     |  __contains__(...)
     |      x.__contains__(y) <==> y in x.
     |  
     |  __eq__(self, value, /)
     |      Return self==value.
     |  
     |  __ge__(self, value, /)
     |      Return self>=value.
     |  
     |  __getattribute__(self, name, /)
     |      Return getattr(self, name).
     |  
     |  __gt__(self, value, /)
     |      Return self>value.
     |  
     |  __iand__(self, value, /)
     |      Return self&=value.
     |  
     |  __init__(self, /, *args, **kwargs)
     |      Initialize self.  See help(type(self)) for accurate signature.
     |  
     |  __ior__(self, value, /)
     |      Return self|=value.
     |  
     |  __isub__(self, value, /)
     |      Return self-=value.
     |  
     |  __iter__(self, /)
     |      Implement iter(self).
     |  
     |  __ixor__(self, value, /)
     |      Return self^=value.
     |  
     |  __le__(self, value, /)
     |      Return self<=value.
     |  
     |  __len__(self, /)
     |      Return len(self).
     |  
     |  __lt__(self, value, /)
     |      Return self<value.
     |  
     |  __ne__(self, value, /)
     |      Return self!=value.
     |  
     |  __or__(self, value, /)
     |      Return self|value.
     |  
     |  __rand__(self, value, /)
     |      Return value&self.
     |  
     |  __reduce__(...)
     |      Return state information for pickling.
     |  
     |  __repr__(self, /)
     |      Return repr(self).
     |  
     |  __ror__(self, value, /)
     |      Return value|self.
     |  
     |  __rsub__(self, value, /)
     |      Return value-self.
     |  
     |  __rxor__(self, value, /)
     |      Return value^self.
     |  
     |  __sizeof__(...)
     |      S.__sizeof__() -> size of S in memory, in bytes
     |  
     |  __sub__(self, value, /)
     |      Return self-value.
     |  
     |  __xor__(self, value, /)
     |      Return self^value.
     |  
     |  add(...)
     |      Add an element to a set.
     |      
     |      This has no effect if the element is already present.
     |  
     |  clear(...)
     |      Remove all elements from this set.
     |  
     |  copy(...)
     |      Return a shallow copy of a set.
     |  
     |  difference(...)
     |      Return the difference of two or more sets as a new set.
     |      
     |      (i.e. all elements that are in this set but not the others.)
     |  
     |  difference_update(...)
     |      Remove all elements of another set from this set.
     |  
     |  discard(...)
     |      Remove an element from a set if it is a member.
     |      
     |      If the element is not a member, do nothing.
     |  
     |  intersection(...)
     |      Return the intersection of two sets as a new set.
     |      
     |      (i.e. all elements that are in both sets.)
     |  
     |  intersection_update(...)
     |      Update a set with the intersection of itself and another.
     |  
     |  isdisjoint(...)
     |      Return True if two sets have a null intersection.
     |  
     |  issubset(...)
     |      Report whether another set contains this set.
     |  
     |  issuperset(...)
     |      Report whether this set contains another set.
     |  
     |  pop(...)
     |      Remove and return an arbitrary set element.
     |      Raises KeyError if the set is empty.
     |  
     |  remove(...)
     |      Remove an element from a set; it must be a member.
     |      
     |      If the element is not a member, raise a KeyError.
     |  
     |  symmetric_difference(...)
     |      Return the symmetric difference of two sets as a new set.
     |      
     |      (i.e. all elements that are in exactly one of the sets.)
     |  
     |  symmetric_difference_update(...)
     |      Update a set with the symmetric difference of itself and another.
     |  
     |  union(...)
     |      Return the union of sets as a new set.
     |      
     |      (i.e. all elements that are in either set.)
     |  
     |  update(...)
     |      Update a set with the union of itself and others.
     |  
     |  ----------------------------------------------------------------------
     |  Static methods defined here:
     |  
     |  __new__(*args, **kwargs) from builtins.type
     |      Create and return a new object.  See help(type) for accurate signature.
     |  
     |  ----------------------------------------------------------------------
     |  Data and other attributes defined here:
     |  
     |  __hash__ = None


​    


```python
s.__contains__(100)
```




    False




```python
# équivalent à :
100 in s
```




    False




```python
s.discard(100)
s.discard(60)
print(s)
```

    {80, 90, 77}


help(...) permet d'obtenir les méthodes d'un objet.  
set.discard(...) retire l'élément s'il est dans le set ou ne fait rien s'il n'y est pas.


```python
help(t)
```

    Help on tuple object:
    
    class tuple(object)
     |  tuple(iterable=(), /)
     |  
     |  Built-in immutable sequence.
     |  
     |  If no argument is given, the constructor returns an empty tuple.
     |  If iterable is specified the tuple is initialized from iterable's items.
     |  
     |  If the argument is a tuple, the return value is the same object.
     |  
     |  Methods defined here:
     |  
     |  __add__(self, value, /)
     |      Return self+value.
     |  
     |  __contains__(self, key, /)
     |      Return key in self.
     |  
     |  __eq__(self, value, /)
     |      Return self==value.
     |  
     |  __ge__(self, value, /)
     |      Return self>=value.
     |  
     |  __getattribute__(self, name, /)
     |      Return getattr(self, name).
     |  
     |  __getitem__(self, key, /)
     |      Return self[key].
     |  
     |  __getnewargs__(self, /)
     |  
     |  __gt__(self, value, /)
     |      Return self>value.
     |  
     |  __hash__(self, /)
     |      Return hash(self).
     |  
     |  __iter__(self, /)
     |      Implement iter(self).
     |  
     |  __le__(self, value, /)
     |      Return self<=value.
     |  
     |  __len__(self, /)
     |      Return len(self).
     |  
     |  __lt__(self, value, /)
     |      Return self<value.
     |  
     |  __mul__(self, value, /)
     |      Return self*value.
     |  
     |  __ne__(self, value, /)
     |      Return self!=value.
     |  
     |  __repr__(self, /)
     |      Return repr(self).
     |  
     |  __rmul__(self, value, /)
     |      Return value*self.
     |  
     |  count(self, value, /)
     |      Return number of occurrences of value.
     |  
     |  index(self, value, start=0, stop=9223372036854775807, /)
     |      Return first index of value.
     |      
     |      Raises ValueError if the value is not present.
     |  
     |  ----------------------------------------------------------------------
     |  Static methods defined here:
     |  
     |  __new__(*args, **kwargs) from builtins.type
     |      Create and return a new object.  See help(type) for accurate signature.


​    


```python
t_2 = t.__add__((100,))
print(t_2)
```

    (77, 80, 90, 100, 100, 100)



```python
# équivalent à
t_3 = t + (100,)
print(t_3)
```

    (77, 80, 90, 100, 100, 100)


#### les méthode `__meth__` sont en fait des surcharges d'opérateur en Python 

`t + 100` ou `t + (100)` tenterais d'additionner un tuple avec un int, on doit donc écrire `(100,)`.
L'embigüité disparait avec `t + (100, 200)`


```python
print(t[0], l[0])
```

    77 77



```python
print(s[0])
```


    ---------------------------------------------------------------------------
    
    TypeError                                 Traceback (most recent call last)
    
    <ipython-input-29-5605e14cbe49> in <module>
    ----> 1 print(s[0])


    TypeError: 'set' object does not support indexing


Les set ne sont pas accéssible par des indexes

## Assignement a[0] = "something"


```python
l[1] = 123
print(l)
```

    [77, 123, 90, 100]



```python
l[9] = 666
print(l)
```


    ---------------------------------------------------------------------------
    
    IndexError                                Traceback (most recent call last)
    
    <ipython-input-32-31f9e3345e18> in <module>
    ----> 1 l[9] = 666
          2 print(l)


    IndexError: list assignment index out of range



```python
t[1] = 22
```


    ---------------------------------------------------------------------------
    
    TypeError                                 Traceback (most recent call last)
    
    <ipython-input-34-e79639a14281> in <module>
    ----> 1 t[1] = 22


    TypeError: 'tuple' object does not support item assignment


