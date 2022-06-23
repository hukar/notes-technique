# Connection ODBC

```python
import pyodbc
```


```python
con = pyodbc.connect('DSN=hukar')
```

## DSN
c'est le nom que l'on donne dans MySQL Connector/ODBC Data Source Configuration

![](../../img/odbc_03.PNG)


```python
cursor = con.cursor()
cursor.execute("SELECT * FROM perso")
row = cursor.fetchone()
print(row)
```

    (1, 'michel', None)

```python
rows = cursor.fetchall()
for row in rows:
    print(row[0], row[1])
```

    2 Lucienne
    3 Tatiana
    4 Robert
    5 Xavier

```python
import time
t0 = time.time()
cursor = con.cursor()
q = "Select count(*) as Reccount "
q += "from perso" 
cursor.execute(q)
rows = cursor.
```
