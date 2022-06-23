# Installation de flask

`pip` est accessible via la commande `pip3` ou `pip3.8`

Pour simplifier la syntaxe je crée un lien symbolique de `pip` vers `pip3`

```bash
ln -s pip3 pip
```

`fichier_source` -> `fichier_lien_symbolique`

### Mettre à jour `pip`

```bash
pip install --upgrade pip
```



Pour voire les package installé par `pip`

```bash
pip list
```

Pour installer `flask`

```sh
pip install flask
```

```sh
C:\Users\kms>which flask
```
pour `bash` 
```sh
type -a flask
```
```sh
C:\Users\kms\AppData\Local\Programs\Python\Python37-32\Scripts\flask.exe
```



```python
from flask import Flask
```

Un package commence par une minuscule.  
Une classe commence par une majuscule.


```python
print(__name__)
```
```sh
    __main__
```


```python
app = Flask("hukar")

from flask import request

def shutdown_server():
    func = request.environ.get('werkzeug.server.shutdown')
    if func is None:
        raise RuntimeError('Not running with the Werkzeug Server')
    func()

# decorator
@app.route("/")
def home():
    return "hey coco"

# @app.route('/shutdown', methods=['POST'])
@app.route('/shutdown')
def shutdown():
    shutdown_server()
    return 'Server shutting down...'

app.run(port=5014)
```

La procédure pur arrêter le serveur ce trouve sur la doc de Flask:  
http://flask.pocoo.org/snippets/67/

Les routes sont définies par des décorateurs @app.route, la methode (le verbe HTTP) peut être définie en deuxième argument.
