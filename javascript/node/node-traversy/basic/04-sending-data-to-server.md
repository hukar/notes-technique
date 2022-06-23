# 04 Recevoir des données des utilisateurs

L'utilisateur renseigne souvent son `authorization` avec son `token`

```js
console.log(req.headers);
```

```json
{
  authorization: 'gftryryururhrhggdtdysyuushsggfdtdyeyhg',
  'user-agent': 'PostmanRuntime/7.22.0',
  accept: '*/*',
  'cache-control': 'no-cache',
  'postman-token': '6ee2e5ce-9871-44b6-b9ed-596fd2abaf53',
  host: 'localhost:4545',
  'accept-encoding': 'gzip, deflate, br',
  connection: 'keep-alive'
}
```

## Écouter le `stream` : `req.on`

La requête gère les streams de données, elle peut s'y abonner :

```js
let body = [];

    req.on("data", chunk => {
        console.log("chunk :", chunk);
        body.push(chunk);
    }).on("end", () => {
        body = Buffer.concat(body);
        console.log(body);
        console.log(body.toString());
    });
```

Le `buffer` est un objet qui représente un tableau-mémoire de taille fixe.

```bash
chunk : <Buffer 7b 0a 09 22 75 73 65 72 49 64 22 3a 20 31 2c 0a 09 22 69 64 22 3a 20 35 2c 0a 09 22 74 69 74 6c 65 22 3a 20 22 6c 61 62 6f 72 69 6f 73 61 6d 20 6d 6f ... 74 more bytes>

<Buffer 7b 0a 09 22 75 73 65 72 49 64 22 3a 20 31 2c 0a 09 22 69 64 22 3a 20 35 2c 0a 09 22 74 69 74 6c 65 22 3a 20 22 6c 61 62 6f 72 69 6f 73 61 6d 20 6d 6f ... 74 more bytes>

object

{
        "userId": 1,
        "id": 5,
        "title": "laboriosam mollitia et enim quasi adipisci quia provident illum",
        "completed": false
}
```

