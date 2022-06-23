
# Connection Python Filemaker ODBC

## Téléchargement des Driver ODBC pour Filemaker
https://www.filemaker.com/support/downloads/
![upload-odbc.PNG](C:\Users\kms\Google Drive\note-technique\img\upload-odbc.PNG)

- On install les deux drivers.
- On configure un DSN (Data Source Name du même nom en 32 et 64 bits.
- regarder ce pdf : https://www.fmpromigrator.com/support/fmpro_migrator/How_to_Create_FileMaker11_ODBC_DSN_Windows.pdf



#### Dans Filemaker 

- file/sharing/Enable ODBC/JDBC

- Specify users: jd_import

- mdp filemaker => manage/security

  privilege Export User pwd : tokapi48


```python
import pyodbc

```


```python
con = pyodbc.connect('DSN=filemaker_hukar;UID=Admin')
```

> attention de bien spécifier le UID ou le PWD si nécéssaire


```python
cursor = con.cursor()
cursor.execute("SELECT * FROM contact")
row = cursor.fetchone()
print(row)
```

    ('john', '4444', 56.0, None)


Obtenir une ligne d'enregistrement : `cursor.fetchone()`


```python
# cursor = con.cursor()
cursor.execute("SELECT * FROM contact")
rows = cursor.fetchall()
for row in rows:
    print(row)
```

    ('john', '4444', 56.0, None)
    ('ezddy mitchel', '4587', 36.0, None)
    ('Jocelyn', '7896', 25.0, None)
    ('veronique', '2569', 34.0, None)


Obtenir tous les enregistrements : cursor.fetchall()


```python
for row in rows:
    print(row[0],int(row[2]))
```

    john 56
    ezddy mitchel 36
    Jocelyn 25
    veronique 34


On peut accéder à une valeur grâce à l'index de sa colonne en Table ...


```python
import time
t0 = time.time()
cursor = con.cursor()
q = "SELECT count(*) as records"
q += " FROM contact"
cursor.execute(q)
row = cursor.fetchone()
print(row)
print(row.records)
print(time.time() - t0,"secondes écoulées")
```

    (4.0, )
    4.0
    0.0019948482513427734 secondes écoulées


... ou avec l'écriture `.` : `row.record`


```python
cursor = con.cursor()
q = "INSERT INTO contact (name, age) VALUES ('sabrina', 32)"
cursor.execute(q)
```




    <pyodbc.Cursor at 0x23733444120>




```python
cursor.execute("SELECT * FROM contact")
rows = cursor.fetchall()
for row in rows:
    print(row)
```

    ('john', '4444', 56.0, None)
    ('michel', '4587', 36.0, None)
    ('Jocelyn', '7896', 25.0, None)
    ('katelyn', '2569', 34.0, None)
    ('micho', '7894', 46.0, None)
    ('micho', '7894', 46.0, None)
    ('sabrina', None, 32.0, None)


