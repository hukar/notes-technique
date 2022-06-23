# 05 Les verbes `HTTP`

## GET

### Retrouver une ressource

## POST

### Ajouter une nouvelle ressource

## PUT / PATCH

### `PUT` modifie toute la ressource

### `PATCH` modifie seulement une partie

## DELETE

### Détruire une ressource

## Restful API Standard

```bash
GET /todos
GET /todos/1
POST /todos
PUT /todos/1
DELETE /todos/1
```

## Exemple de gestion de `POST`

```js
if (method === "POST" && url === "/todos") {
  // JSON.parse works with Buffer instance, no need of toString
  const { id, title } = JSON.parse(body);
	
  // check fields received
  if (!id || !title) {
    status = 400;
  } else {
    todos.push({ id, title });
    status = 201;
    response.success = true;
    response.data = todos;
  }
}
```

`code 400 : Bad request`

