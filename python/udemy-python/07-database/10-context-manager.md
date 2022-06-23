# 10 Contexte Manager

Dunder method = Double Under `__method__`

`Dunder` ou `magic methods` en python désigne les méthodes ayant en prefixe et en suffixe deux underscores dans leur nom. Dunder signifie ici “Double Under (Underscores)”.

## Context Manager

L'utilisation du `context manager` permet de simplifier le code et de le rendre plus lisible.

```python
with ... as ...: # initialisation du contexte
    pass # le contexte en lui-même
```

# Classe de connection à la DB utilisant le contexte

```python
import sqlite3

class DatabaseConnection:
    # when the object is create
    def __init__(self, host):
        self.connection = None
        self.host = host

    # go into the context manager
    def __enter__(self):
        self.connection = sqlite3.connect(self.host)
        return self.connection

    #leave from the context manager
    def __exit__(self, exc_type, exc_val, exc_tb):
        # if nothing to commit sqlite do nothing it's fine
        self.connection.commit()
        self.connection.close()
```

On utilise trois méthodes ***Dunder***:

`__init__`  qui s'exécute quand l'objet est créé.
`__enter__` qui s'exécute quand on entre dans le contexte

`__exite__` qui s'exécute lorsque l'on sort du contexte

## Utilisation dans le programme

```python
from database_connection import DatabaseConnection
# relative import
# from .database_connection import DatabaseConnection

def create_book_table():
    # création de l'objet puis entrée dans le contexte
    with DatabaseConnection('data.db') as connection:
        cursor = connection.cursor()
        # IF NOT EXISTS
        cursor.execute('CREATE TABLE IF NOT EXISTS books(name text primary key, author text, read integer)')
# sortie du contexte

def add_book(name, author):
    with DatabaseConnection('data.db') as connection:
        cursor = connection.cursor()

        cursor.execute('INSERT INTO books VALUES(?, ?, 0)', (name, author))


def get_all_books():
    with DatabaseConnection('data.db') as connection:
        cursor = connection.cursor()

        cursor.execute('SELECT * FROM books')

        books = [{"name": row[0], "author": row[1], "read": row[2]} for row in cursor.fetchall()]

    return books


def read_book(name):
    with DatabaseConnection('data.db') as connection:
        cursor = connection.cursor()

        cursor.execute('UPDATE books SET read=1 WHERE name=?',(name,))


def delete_book(name):
    with DatabaseConnection('data.db') as connection:
        cursor = connection.cursor()

        cursor.execute('DELETE FROM books WHERE name=?', (name,))


```

