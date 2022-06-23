# Connection ODBC avec Python

## 1. installer ODBC pour MySQL

a. télécharger le Driver ODBC MySQL pour son système

https://dev.mysql.com/downloads/connector/odbc/

b. Configurer le Driver MySQL dans **`ODBC Data Source Administrator`** (pour **Windows** en mode admin)

explication : https://dev.mysql.com/doc/connector-odbc/en/connector-odbc-configuration-dsn-windows-5-2.html

![](..\..\img\odbc_01.PNG)

![](../../img\odbc_02.PNG)

![](../../img\odbc_03.PNG)

## 2. installer pyodbc

Il suffit d'utiliser `pip`  :

```sh
pip install pyodbc
```

On retrouve le package à l'adresse :

`C:\Users\kms\AppData\Local\Programs\Python\Python37-32\Lib\site-packages\pyodbc-4.0.25.dist-info`

