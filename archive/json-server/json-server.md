# Json-server

## installation

```bash
npm install -g json-server
```

## Utilisation

créer à la racine de votre projet un fichier par exemple `database.json` :

```json
{
    "users": [
        {
            "id": 0,
            "name": "kiki"
        },
        {
            "id": 1,
            "name": "coco"
        }
    ],
    "projects": [
        {
            "id": 0,
            "name": "picasso",
            "email": "p@g.c",
            "status": "Stable"
        },
        {
            "id": 1,
            "name": "miro",
            "email": "m@r.",
            "status": "Critical"
        },
        {
            "id": 1,
            "name": "kandinski",
            "email": "kan@fred.be",
            "status": "Finished"
        }
    ]
}
```

dans le terminal :

```bash
json-server --watch database.json
```

Ici le terminal nous donne l'adresse de l'API: `http://localhost:3000`

## Avec Angular et HttpClient

### GET

```js
getHttp() {
    const userUrl = 'http://localhost:3000/users';
    
    this.http.get(userUrl)
        .subscribe(
        x => console.log(x)
    );
}
```

### POST

Va écrire dans notre fichier `database.json` ```(´♡‿♡`)/```

Les données sont donc enregistrées en dure, c'est génial !

```js
postHttp() {
    const userUrl = 'http://localhost:3000/users';

    const u: User = {
        id: 3,
        name: 'John'
    };

    const headers = new HttpHeaders({'Content-Type': 'application/json'});

    this.http.post(userUrl, u, {headers: headers})
        .subscribe(
        x => console.log(x)
    );
}
```

Il faut régler le header avec `Content-Type: application/json`

Si on tente d'enregistrer deux fois un élément avec le même id on obtient une erreur :

```bash
"Error: Insert failed, duplicate id ... "
```

### PUT

De nouveau cela écrit réellement dans `database.json`

```js
putHttp() {
    const userUrl = 'http://localhost:3000/users/2'; // <- ici on met l'id

    const headers = new HttpHeaders({'Content-Type': 'application/json'});


    this.http.put(userUrl, {name: 'enrico'}, {headers: headers})
        .subscribe(
        x => console.log(x)
    );
}
```

