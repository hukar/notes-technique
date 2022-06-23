# 03 Les codes de status `HTTP`

## 1.XX Informatif

## 2.XX Succès

200 Success

201 Created

204 No Content

## 3.XX Redirection

304 not Modified

## 4.XX Client Error

400 Bad Request

401 Unauthorized

404 Not Found

## 5.XX Server Error

500 Internal Server Error

## Préciser le status de la réponse `statusCode`

```js
res.statusCode = 404;
res.setHeader("Content-Type", "application/json");

res.end(
  JSON.stringify({
    success: false,
    error: "not found",
    data: null
  })
);
```

## Écriture raccourcie `writeHead`

```js
res.writeHead(401, {
        "Content-Type": "application/json",
        "X-Powered-By": "node.js"
    });
```

