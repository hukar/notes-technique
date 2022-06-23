# 07 SQLite Mise à jour des données

```python
def read_book(name):
    connection = sqlite3.connect('data.db')
    cursor = connection.cursor()

    cursor.execute('UPDATE books SET read=1 WHERE name=?',(name,))
    connection.commit()
    connection.close()
```

```sql
UPDATE books SET name="titi" WHERE id=5
```

