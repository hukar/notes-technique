# 05 Ajouter des données dans SQLite

1. On seconnecte à la BDD `connection = sqlite3.connect('data.db')`
2. On récupère le pointeur `cursor = connection.cursor()`
3. On mémorise une requête `cursor.execute('REQUETE')`
4. On exécute les requêtes enregistrées `connection.commit()`
5. On ferme la connection `connection.close()`

```python
import sqlite3

def add_book(name, author):

    connection = sqlite3.connect('data.db')
    cursor = connection.cursor()

    # be careful to the double quote mandatory
    # cursor.execute(f'INSERT INTO books VALUES("{name}", "{author}", 0)') => too dangerous sql injection
    cursor.execute('INSERT INTO books VALUES(?, ?, 0)', (name, author))
    # integer real text null blob
    connection.commit()
    connection.close()
```

Les doubles quotes sont obligatoires pour qu'il n'y ait pas d'ambigüité avec le nom des tables.

```sql
INSERT INTO table_name VALUES( ... , ... , ... )
```

On passe un `tuple` en deuxième argument de `excecute`

