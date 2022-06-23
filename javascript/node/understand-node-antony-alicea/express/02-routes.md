# 02 Les routes dans express

```js
app.get("/api", (req, res) => {
    res.json({ name: "jin", age: 48 });
});
```

## Une route avec paramètre(s) utilisation de `:`

```js
app.get("/person/:id", (req, res) => {
    res.send(`<html><body><h1>Person : ${req.params.id}</h1></body></html>`);
});
```

Les paramètres sont contenus dans `req.params`.

## Avec deux paramètres

```js
app.get("/person/:age/:name", (req, res) => {
    const { name, age } = req.params;
    res.send(`${name} have ${age}`);
});
```

