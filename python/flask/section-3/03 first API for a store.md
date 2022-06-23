
# 03 première API pour un store

Pour afficher correctement le json installer l'exrtension pour Chrome `JSONView`

#### Le format JSON utilise **uniquement** les guillemets doubles


```python
if 1 < 2:
    print("hello")
```

    hello



```python
d = {'a': 'aaa'}
print(d['a'])
```

    aaa


jsonify doit prende un dictionnaire en paramètre.

## Le code de l'API 


```python
from flask import Flask, jsonify, request

app = Flask(__name__)
```

Import de Flask


```python
stores = [
    {
        'name': 'my little shop',
        'items': [
            {
                'name': 'puma shoes',
                'price': 49.90
            },
            {
                'name': 'glow',
                'price': 25.49
            }
        ]
    }
]
```

représentation des stores avec une liste de dictionnaire.


```python
#POST used to receive data
#GET used to send data back only

#POST /store data: {name:}
#methods['POST', 'GET'] alternative
@app.route('/store', methods=['POST'])
def create_store():
    request_data = request.get_json()
    # on crée un dictionnaire pour jsonify
    new_store = {
        'name': request_data['name'],
        'items': []
    }
    stores.append(new_store)
    return jsonify(new_store)
```

```
@app.route('/store', methods=['POST'])
```
décorateur pour le endpoint /store avec la méthode POST.  
la liste ``methods`` peut avoir plusieurs verbes HTTP.  


```python
#GET /store/<string:name>
@app.route('/store/<string:name>')
def get_store(name):
    for store in stores:
        if store['name'] == name:
            return jsonify(store)
    return jsonify({'error':f"no store named {name}"})
```

### ``<string:name>`` passage de paramètre par l'URL  
``jsonify`` transforme le dictionnaire en json (texte)


```python
#GET /store
@app.route('/store')
def get_stores():
    return jsonify({'stores': stores}) # jsonify accept only dictionnary
```


```python
#POST /store/<string:name>/item {name:,price:}
@app.route('/store/<string:name>/item', methods=['POST'])
def create_item_in_store(name):
    for store in stores:
        if store['name'] == name:
            request_data = request.get_json()
            new_item = {
                'name': request_data['name'],
                'price': request_data['price']
            }
            store['items'].append(new_item)
            return jsonify(store)
    return jsonify({'error': f"store {name} not found"})
```

Récupération des données envoyées avec ``request.get_json()``


```python
#GET /store/<string:name>/item
@app.route('/store/<string:name>/item')
def get_item_in_store(name):
    for store in stores:
        if store['name'] == name:
            return jsonify({'items': store['items']})
    return jsonify({'error': f"store {name} not found"})

app.run(port=5066)
```



