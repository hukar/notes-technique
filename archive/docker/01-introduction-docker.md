# 01 Introduction Docker

Docker hub = registre image officiel docker

Plusieurs **Repl** très utililes

```sh
docker container run -ti python:3
```

exemple avec mongoDB

```sh
docker container run -p 27017:27017 -d mongo:4.0
```

`-p`  permet de choisir le port

`-d`  lance mongoDB en tâche de fond (?)

`compass` pour administrer mongoDB

`Redmine` = gestion de projet

```sh
docker container run -p 80:3000 redmine:3
```

`-p 80:3000`  port 3000 dans le container et port 80 sur notre machine

## Lancer une stack logstash-kibana-elasticsearch

```sh
docker-compose up -d
```

un fichier `lke.yml` permet de composer le container

## Autre façon :

```sh
docker stack deploy -c tick.yml tick
```

lance l'application sur un ensemble de machines

## Concepts utils

### Container Linux

- un processus

- Isolé

- Namespaces (propre vision du système)
- Control Groups (limite ressources)
- partage du kernel avec les autres containers

#### Différent Namespace

- pid :
- net :
- mount :
- uts :
- ipc :
- user :