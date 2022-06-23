# 03 renvoyer du `Json`

## Réponses `HTTP`

<img src="assets/req-http-squelette.png" alt="req-http-squelette" style="zoom:50%;" />

Le type `MIME` : `application/json` (pour une page web c'est `text/html`)

## `res.json`

Transforme la réponse en `json` valide et ajoute le type `MIME` `application/json` :

<img src="assets/header-content-type.png" alt="header-content-type" style="zoom:50%;" />

```js
app.get('/api/pokemons/:id/', (req, res) => {
    const id = parseInt(req.params.id)
    const pokemon = pokemons.find((pokemon) => pokemon.id === id)
    if (pokemon) {
        res.json(pokemon)
    } else {
        res.status(404)
        res.send('pokemon non repertorié')
    }
})
```



## `helpers.js`

```js
const success = (message, data) => {
  return {
    message: message,
    data: data,
  }
}

exports.success
```

























