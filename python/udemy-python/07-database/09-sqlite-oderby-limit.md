# 09 ORDER BY et LIMIT avec SQLite

```python
import sqlite3
from datetime import  datetime

def create_table():
    connection = sqlite3.connect('order_by.db')
    cursor = connection.cursor()

    cursor.execute('CREATE TABLE IF NOT EXISTS purchases (id integer primary key,amount real,date integer)')

    connection.commit()
    connection.close()

def add_purchase(amount):
    now = datetime.now()
    timestamp = int(datetime.timestamp(now)) # 1551344987

    connection = sqlite3.connect('order_by.db')
    cursor = connection.cursor()

    cursor.execute('INSERT INTO purchases (amount, date) VALUES (?, ?)', (amount, timestamp))

    connection.commit()
    connection.close()

def display_purchases():
    connection = sqlite3.connect('order_by.db')
    cursor = connection.cursor()

    cursor.execute('SELECT * FROM purchases ORDER BY date DESC LIMIT 3')

    for purchase in cursor:
        print(f'{purchase[0]} | {purchase[1]}\t| {purchase[2]}')

# create_table()

# add_purchase(109.40)
# add_purchase(9.86)
# add_purchase(150.00)
# add_purchase(0.55)

display_purchases()

```

```shell
6 | 109.4	| 1551344987
5 | 17.23	| 1551344976
1 | 19.99	| 1551344951
```

