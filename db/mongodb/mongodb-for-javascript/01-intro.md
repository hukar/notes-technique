# 01 Mongo DB University : mongo for `js`



## `URI` : `U`niform `R`essource `I`dentifier

<img src="assets/uri-mongodb-connect.png" alt="uri-mongodb-connect" style="zoom:50%;" />

<img src="assets/uri-connection-string.png" alt="uri-connection-string" style="zoom:50%;" />

### `srv` string

```
mongodb+srv://<username>:<password>@<host>/<database>
```

<img src="assets/srv-record-schema.png" alt="srv-record-schema" style="zoom:50%;" />

C'est un fichier `srv` qui est contenu dans un `dns` et qui connait l'adresse physique des `clusters`.

Cela permet de ne rien changer côté client si jamais un cluster était remplacé par un autre.

On peut spécifier des options à la fin après un point d'interrogation :

```
mongodb+srv://USERNAME:PASSWORD@Mxxx-zzzz-mcxlm.mongodb.net/admin?retryWrites=true
```



## Free Atlas Account

name : `mflix`

`Load sample dataset`

`Connect` : white list my `IP adress`

Si on veut autoriser toutes les adresse IP on tape :

```
0.0.0.0/0
```

Ce n'est pas une bonne pratique.

Username : `m220student`

Password : `m220password`

Installer `Compass`. (`connect`)

Cliquer sur `favourite`.



### En local

On peut charger les données en local grace à la commande :

```bash
mongorestore --gzip --archive=sampledata.archive.gz
```

