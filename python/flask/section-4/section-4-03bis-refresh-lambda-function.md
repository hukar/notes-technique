# Section 4.03 bis Les fonctions Lambda

```python
my_f = lambda other_f, v: other_f(v + 5)

print(my_f(lambda x: x**2, 2)) # 49
```

Les fonction lambda sont de la forme :

```pseudocode
lambda [parameter_list] ":" expression
```

Ce qui est l'équivalent de :

```pseudocode
def <lambda>(parameters):
    return expression
```

#### ! les lambda n'accepte que des expression (retour d'une valeur) et pas des instruction (calcul d'une valeur)

Les fonctions lambda ne peuvent pas être sur plusieurs lignes.

notes de Guido van Rossum the creator of Python :  Machine de Rube Goldberg :

https://www.artima.com/weblogs/viewpost.jsp?thread=147358

en résumé ***pas d'indentation dans une expression***

## Programmation Fonctionnel

```python
l = [34,56,79,43]

ll = list(filter(lambda x: x != 34, l))
lll= list(filter(lambda x: x%2 != 0, l))
print(ll)
print(lll)

lc = [item for item in l if item % 2 != 0]
print(lc)
```

Dernière solution avec les listes de compréhension.

## Bien comprendre la syntaxe des lambda :

```python
(lambda x, y: x*3 + y*2)(1, 2) # 7

def f(x, y):
    return x*3 + y*2

f(1, 2) # 7
```

