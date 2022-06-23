# 01 Exploration de `MongoDB`

`db` montre la base de données active.

```js
> db
test
```

`show dbs` liste toutes les `bdd` du serveur.

```js
> show dbs
admin   0.000GB
config  0.000GB
local   0.000GB
```

`use <database name>` switch sur une autre `bdd`, elle devient active.

```js
> use local
switched to db local
> db
local
```

`show collections` liste les collections dans la `bdd` active.

```js
> show collections
startup_log
> use admin
switched to db admin
> show collections
hukar_test
system.version
> use config
switched to db config
> show collections
system.sessions
```

