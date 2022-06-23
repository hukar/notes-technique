# 02 Connexion au cluster de MongoDb Atlas

## Connection aux `dbs` d'exemple

```bash
mongo "mongodb://cluster0-shard-00-00-jxeqq.mongodb.net:27017,cluster0-shard-00-01-jxeqq.mongodb.net:27017,cluster0-shard-00-02-jxeqq.mongodb.net:27017/test?replicaSet=Cluster0-shard-0" --authenticationDatabase admin --ssl --username m001-student --password m001-mongodb-basics
```

```bash
PRIMARY> show dbs
100YWeatherSmall  0.128GB
admin             0.000GB
aggregations      0.067GB
citibike          0.367GB
city              0.002GB
config            0.000GB
coursera-agg      0.083GB
local             0.940GB
mflix             0.449GB
results           0.000GB
ships             0.001GB
video             0.513GB
```

## Seulement sur `100YWeathreSmall`

```bash
mongo "mongodb://cluster0-shard-00-00-jxeqq.mongodb.net:27017,cluster0-shard-00-01-jxeqq.mongodb.net:27017,cluster0-shard-00-02-jxeqq.mongodb.net:27017/100YWeatherSmall?replicaSet=Cluster0-shard-0" --authenticationDatabase admin --ssl --username m001-student --password m001-mongodb-basics
```

On voit que la communication utilise `SSL`

## Voire les collections `show`

```bash
> show collections
data
system.profile
```

## Changer de collection `use`

```bash
> use video
switched to db video
```

```bash
> show collections
movies
```

## Voire les enregistrement `find()`

```bash
db.movies.find().pretty()
```

