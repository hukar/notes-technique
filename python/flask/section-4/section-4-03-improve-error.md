# Section 4.03 amélioration et gestion des erreurs

## utilisation de `request.get_json`

```python
from flask import Flask, request
# ...

class Item(Resource):
# ...

def post(self, name):
    # if Content-Type isn't application/json will get an error
    data = request.get_json()
    # data = request.get_json(force=True) force the conversion without Content-Type setted
    # data = request.get_json(silent=True) Don't send an error and do nothing
    item = {
        'name': name,
        'price': data['price']
    }
    items.append(item)
    return item, 201 # 201 CREATED
```

## data = request.get_json()

Si le contenu n'est pas du `json` valide :

```json
// json envoyé avec '=' au lieu des ':'
{
	"price"= 20.99
}
```



```sh
{
    "message": "Failed to decode JSON object: Expecting ':' delimiter: line 2 column 9 (char 10)"
}
```

Avec `get_json(silent=True)` on obtient le grand message d'erreur du Debugger

## Sans le Content-Type

`get_json(force=True)` accepte le contenu

####! cela peut-être dangereux d'accepter des données sans header configuré