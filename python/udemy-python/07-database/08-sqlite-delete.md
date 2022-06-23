# 08 SQLite éffacer des données

```python
def delete_book(name):
    connection = sqlite3.connect('data.db')
    cursor = connection.cursor()

    cursor.execute('DELETE FROM books WHERE name=?', (name,))

    connection.commit()
    connection.close()
```

```sql
DELETE FROM books WHERE name="toto"
```

