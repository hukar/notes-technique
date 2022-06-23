
# Set opérations avancé 

## set opérations 

### Intersection 


```python
s1 = {1,2,3,4,5,6}
s2 = {1,2,5,6,7,9,11}

its = s1.intersection(s2)
print(its)
```

    {1, 2, 5, 6}
    

### Union 


```python
s3 = set([1, 5, 11, 56])
u = s1.union(s3)
print(u)
```

    {1, 2, 3, 4, 5, 6, 11, 56}
    

### Différence 


```python
print({1,2,3,4}.difference({1,2}))
```

    {3, 4}
    
