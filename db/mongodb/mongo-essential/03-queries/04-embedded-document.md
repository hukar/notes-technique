# 04 `documents` imbriqués (`embedded`)

```js
db.persons
.find({company.title: "YURTURE"})
```

```js
Error: Line 2: Unexpected token .
```

L'écriture avec `.` rend obligatoire l'utilisation de guillemets :

```js
db.persons
.find({"company.title": "YURTURE"})
```

```js
/* 1 */
{
    "_id" : ObjectId("5e8dd16bada0697e5173a466"),
    "index" : 0,
    "name" : "Aurelia Gonzales",
    // ...
```

```js
db.persons
.find({"company.title": "YURTURE"})
.count()

2
```

On peut demander exactement un `document` avec un sous-`document` précis :

```js
db.persons
.find({"company": {
        "title" : "YURTURE",
        "email" : "aureliagonzales@yurture.com",
        "phone" : "+1 (940) 501-3963",
        "location" : {
            "country" : "USA",
            "address" : "694 Hewes Street"
        }
    }})
.count()

1
```

Bien sûr on obtient un seul `document`.

#### ! Problème : l'ordre des champs compte !

```js
db.persons
.find({"company": {
        "title" : "YURTURE",
        "email" : "aureliagonzales@yurture.com",
        "phone" : "+1 (940) 501-3963",
        "location" : {
            "address" : "694 Hewes Street", // intervertit avec le précédent
            "country" : "USA"         
        }
    }})
.count()

0
```

Du coup on évite d'utiliser cette syntaxe !

### Chaîne d'objets imbriqués

```js
db.persons
.find({"company.location.country":"USA"})
.count()

255
```

