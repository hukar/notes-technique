# 08 Projection

On peut vouloir récupérer seulement quelques champs, on fait donc une projection :

```bash
PRIMARY> db.movies.find({genre: "Action, Adventure"}, {title: 1})
```

```bash
# ...
{ "_id" : ObjectId("58c59c6b99d4ee0af9e0efa9"), "title" : "The Lost City" }
{ "_id" : ObjectId("58c59c6b99d4ee0af9e0f0b7"), "title" : "The Screaming Shadow" }
{ "_id" : ObjectId("58c59c6b99d4ee0af9e0f0cf"), "title" : "The Son of Tarzan" }
{ "_id" : ObjectId("58c59c6b99d4ee0af9e0f1a1"), "title" : "The Adventures of Tarzan" }
Type "it" for more
```

**MongoDB** affiche les 20 premiers résultat, pour déplacer le curseur (`iterator`), on peut taper `it` pour avoir les 20 suivants.

Par défaut une projection conserve toujours l'`_id`, si on veut inclure un champs on lui donne la valeur `1`, si on veut l'exclure la valeur `0`.

```bash
PRIMARY> db.movies.find({genre: "Action, Adventure"}, {title: 1, _id: 0})
```

```bash
{ "title" : "Who Will Marry Mary?" }
{ "title" : "The New Exploits of Elaine" }
{ "title" : "The Ventures of Marguerite" }
{ "title" : "The Iron Claw" }
# ...
```

