# 03 Enregistrement dans un fichier en json

Utilisons un fichier `books.json` pour sauvegarder nos livres.

## Utilisation du json

```python
"""
creating and retrieving book from a json file.
[
    {
        'name': 'Clean Code',
        'author': 'Robert'
        'read': False
    }
]
"""
import json
```
importation du module json
```python
books_file = "books.json"

def create_book_table():
    with open(books_file, 'w') as file:
        json.dump([], file)
```
Pour être valide un fichier vide json en Python doit contenir un tableau vide.  
Dans d'autres langages, il faut un objet vide `{ }` .
json.dump(array_of_dict, file) copie et transforme en json le tableau de dictionnaire dans le fichier spécifié par file.
```python
def add_book(name, author):
    books = get_all_books()
    books.append({'name': name, 'author': author, 'read': False})
    _save_all_books(books)

def get_all_books():
    with open(books_file, 'r') as file:
        return json.load(file)
```
`json.load(file)` renvoie un fichier json sous la forme d'un tableau de dictionnaires `[ {...}, {...} ]`

```python
def _save_all_books(books):
    with open(books_file, 'w') as file:
        json.dump(books, file)

def read_book(name):
    books = get_all_books()

    # search and pass to 1 the book read
    for book in books:
        if book['name'] == name:
            book['read'] = True
            break
    _save_all_books(books)

def delete_book(name):
    books = get_all_books()
    books = [book for book in books if book['name'] != name]
    _save_all_books(books)
```

