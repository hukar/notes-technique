# 06 `Restful API`

C'est un style architecturale.

`REST` : `RE`presentational `S`tate `T`ransfer

On donne du sens à l'`URL` et au verbe `HTTP`.

```js
app.get("/api/person/:id", (req, res) => {
    // get that data from database
});

app.post("/api/person", jsonParser, (req, res) => {
    // save person in the database
});

app.delete("api/person/:id", (req, res) => {
    // delete person from database
});
```

Exemple de `RESTFul API`.

