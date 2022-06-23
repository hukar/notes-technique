# 00 Utilisation de `brew services` pour lancer `mongod`

### Lister les services en cours `brew services list`

```bash
$ brew services list

Name              Status  User Plist
mongodb-community started kar  /Users/kar/Library/LaunchAgents/homebrew.mxcl.mongodb-community.plist
```

### Relancer un services `brew services restart`
```bash
kar : ~ $ brew services restart mongodb-community

Stopping `mongodb-community`... (might take a while)
==> Successfully stopped `mongodb-community` (label: homebrew.mxcl.mongodb-community)
==> Successfully started `mongodb-community` (label: homebrew.mxcl.mongodb-community)
```

### Arrêter un service  `brew services stop`

```bash
$ brew services stop mongodb-community

Stopping `mongodb-community`... (might take a while)
==> Successfully stopped `mongodb-community` (label: homebrew.mxcl.mongodb-community)
```

```bash
$ mongo

MongoDB shell version v4.2.3
connecting to: mongodb://127.0.0.1:27017/?compressors=disabled&gssapiServiceName=mongodb
2020-03-08T15:13:50.440+0100 E  QUERY    [js] Error: couldn't connect to server 127.0.0.1:27017, connection attempt failed: SocketException: Error connecting to 127.0.0.1:27017 :: caused by :: Connection refused :
connect@src/mongo/shell/mongo.js:341:17
@(connect):2:6
2020-03-08T15:13:50.442+0100 F  -        [main] exception: connect failed
2020-03-08T15:13:50.442+0100 E  -        [main] exiting with code 1
```

### Démmarer un service `brew services start`
```bash
$ brew services start mongodb-community

==> Successfully started `mongodb-community` (label: homebrew.mxcl.mongodb-community)
```
```bash
$ mongo

MongoDB shell version v4.2.3
connecting to: mongodb://127.0.0.1:27017/?compressors=disabled&gssapiServiceName=mongodb
Implicit session: session { "id" : UUID("a8391258-304d-44a1-86c4-5c4fa2eef239") }
MongoDB server version: 4.2.3
Server has startup warnings: 
2020-03-08T15:14:21.249+0100 I  CONTROL  [initandlisten] 
2020-03-08T15:14:21.249+0100 I  CONTROL  [initandlisten] ** WARNING: Access control is not enabled for the database.
2020-03-08T15:14:21.249+0100 I  CONTROL  [initandlisten] **          Read and write access to data and configuration is unrestricted.
2020-03-08T15:14:21.249+0100 I  CONTROL  [initandlisten] 
2020-03-08T15:14:21.249+0100 I  CONTROL  [initandlisten] 
2020-03-08T15:14:21.249+0100 I  CONTROL  [initandlisten] ** WARNING: soft rlimits too low. Number of files is 256, should be at least 1000
---
Enable MongoDB's free cloud-based monitoring service, which will then receive and display
metrics about your deployment (disk utilization, CPU, operation statistics, etc).

The monitoring data will be available on a MongoDB website with a unique URL accessible to you
and anyone you share the URL with. MongoDB may use this information to make product
improvements and to suggest MongoDB products and deployment options to you.

To enable free monitoring, run the following command: db.enableFreeMonitoring()
To permanently disable this reminder, run the following command: db.disableFreeMonitoring()
---

> 

```

### Vérifier que le `deamon` est lancé

```bash
$ ps aux | grep -v grep | grep mongod

kar              34165   1.4  0.2  5605864  38848   ??  S     3:14PM   0:02.55 /usr/local/opt/mongodb-community/bin/mongod --config /usr/local/etc/mongod.conf
```

`-v` renvoyer les ligne qui ne correspondent pas au motif 

`man grep` :

```bash
     -v, --invert-match
             Selected lines are those not matching any of the specified patterns.
```

