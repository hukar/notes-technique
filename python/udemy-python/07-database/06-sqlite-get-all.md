# 06 Récupérer des données avec SQLite

```python
def get_all_books():
    connection = sqlite3.connect('data.db')
    cursor = connection.cursor()

    cursor.execute('SELECT * FROM books')

    # convert tuples to dictionnaries
    books = [{"name": row[0], "author": row[1], "read": row[2]} for row in cursor.fetchall()]

    # no need to commit in reading
    connection.close()

    return books
```

`cursor` est un pointeur qui pointe sur la première ligne de la table.

```python
[(name, author, read), (..., ..., ...) , ...]
```



`cursor.fetchall()` renvoie un tableau de tuples.

`cursor. fetchone()` renvoie le premier enregistrement.

`cursor.fetchany(5)` renvoie les cinq premiers enregistrements.

Il n'y a pas besoin de `connection.commit()`.

La liste de comprehension permet de transformer un tableau de tuples en tableau de dictionnaires.

Au lieu d'un accès par indice peut parlant, on accède avec le nom des colonnes

```python
books = get_all_books()
for book in books:
    print(book['name'], book['author'])

# plus parlant que :

for book in books:
    print(book[0], book[1]) # en laissant les tuples
```

