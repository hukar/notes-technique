#  04 SQLite Créer une Table

SQLite stocke toutes les données dans un unique fichier.

= `single data file`

SQL Structured Query Language

**sqlite limitation :** 

- seulement une seule écriture à la fois

- plusieurs lectures simultanées sont possibles.

- Très rapide en lecture, pas si rapide en écriture.

## Visualisation des données sqlitebrowser

## Créer une table

```python
import sqlite3

connection = sqlite3.connect('data.db')
cursor = connection.cursor()

cursor.execute('CREATE TABLE books(name text primary key, author text, read integer)')
# integer real text null blob
connection.commit()
connection.close()
```



5 types disponnibles :

1. integer

2. real
3. text
4. null
5. blob

```sql
CREATE TABLE IF NOT EXISTS table_name(col_name type primary key,col_name type)
```

