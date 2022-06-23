# 02 enregistrement des données au format csv

## Utilisation des fichiers

````python
with open("monfich.txt","mode") as f:
````

mode :

- 'w' écriture (éfface le fichier lors de l'écriture)
- 'a' (append) ajout à la fin du fichier
- 'r' read lecture

## line.strip() et file.readlines()

`line.strip()` retire le saut de ligne.

`file.readlines()` renvoie un tableau de lignes.

## line.split(',')

Transforme une chaîne en tableau, délimiteur en argument.

## ajout d'un enregistrement au format csv

```python
# écrire dans un fichier

my_file = "toto.txt"


def add_item(item, price):
    with open(my_file, 'a') as f:
        f.write(f"{item},{price}\n")


# add_item("chocolat", 0.99)
# add_item("carot",1.23)
# add_item("unicorn", 19999)
```

## Lecture d'un fichier au format csv

```python
def list_item():
    with open(my_file, 'r') as f:
        line = [line.strip().split(',') for line in f.readlines()]
        print(line)


list_item() # [['chocolat', '0.99'], ['carot', '1.23'], ['unicorn', '19999']]
```

`f.read()` renvoie tout le contenu

`f.readlines()` renvoie un tableau de toutes les lignes.

## Convention Python

Si une fonction a un underscore devant elle, elle doit être considérée comme privée.

## Écriture dans un fichier

```python
# save the updated content in the file
    with open(books_file, 'w') as f:
        for book in books:
            f.write(f"{book['name']},{book['author']},{book['read']}\n")
```

## Liste de comprehension pour la supression

````python
def del_book(name):
    books = get_books()
    books = [book for book in books if book['name'] != name]
````

## Découpler l'implémentation

Présenté une interface pour gérer la database permet de découpler le code de l'implémentation de cette interface.

