# Section 4 02 Rest API

exemple d'API créée dans Postman :

```sh
GET /items # resource list of items
GET /item/<name> # resource item
POST /item/<name>
PUT /item/<name>
DELETE /item/<name>
```

Il y a deux ressource: une liste d'items et un item.

Sauf le verbe les quatre URL de Item sont identiques.

#### ! flask-RESTful permet de ne plus utiliser `jsonify` car il le fait en arrière plan.

Les **dictionnaire Python** sont transformé automatiquement en du JSON valide.

## Code

```python
from flask import Flask
from flask_restful import Resource, Api

app = Flask(__name__)

api = Api(app)

items = []

class Item(Resource):

    def get(self, name):
        for item in items:
            if item['name'] == name:
                return item
        
        # be careful for the uppercase N of None
        # None will transformed to null in json
        # The second argument is the server status: 404 NOT FOUND
        return {'item': None}, 404

    def post(self, name):
        item = {
            'name': name,
            'price': 12.90
        }
        items.append(item)
        # the status is for creating resource: 201 CREATED
        # for a delegate creation use this status: 202 ACCEPTED
        return item, 201

api.add_resource(Item, '/item/<string:name>')

app.run() # set the port in 5000 per default
```

> en python une fonction peut retourner plusieurs valeurs :
>
> ```python
> def toto():
>     return 4, 5
> 
> a, b = toto()
> 
> print(b) # 5
> print(a) # 4
> ```

#### ! En deuxième argument du return on peut préciser le status du serveur 200 par défaut.

200 OK

201 CREATED

202 ACCEPTED

404 NOT FOUND

## Pour avoir un message erreur plus complet

`app.run(debug=True)`

envoie un message détaillé au format HTML