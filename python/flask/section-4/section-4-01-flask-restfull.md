# Section 4/ 01 flask-restfull

## Créer un environnement virtuel

```sh
pip freeze
```

```sh
aniso8601==4.1.0
Click==7.0
Flask==1.0.2
Flask-RESTful==0.3.7
itsdangerous==1.1.0
Jinja2==2.10
MarkupSafe==1.1.0
pytz==2018.9
six==1.12.0
virtualenv==16.3.0
Werkzeug==0.14.1
```

Liste les librairies installé par `pip`.

`virtualenv` va permettre de créer de nouvelles applications avec les versions récentes des librairies sans pour autant toucher aux anciennes applications et prendre le risque de les *"casser"*.

#### ! Les différents projets ne doivent pas partager leurs librairies.

## Installer `virtualenv`

```sh
pip install virtualenv
```

## Créer un environnement virtuel

```sh
virtualenv my_virtual_environment --python=python3
```

`--python=`  choisie la version de python dans le PATH, ici `python3` mais dans d'autres installation peut-être `python` ou `python2` ou encore `python3.5` suivant comment sont nommées les versions dans le PATH. 

## Lancer l'environnement virtuel sur Mac

```sh
source my_virtal_environment/bin/activate
```

## Lancer l'environnement virtuel sur le git bash dans windows

lancer `virtualenv` :

```sh
source my_ve/Scripts/activate
```

Lorsque l'environnement virtuel tourne, on a un `(venv)` au début de sa ligne de commande :

```sh
(venv) hukar: section-4 $
```



## arrêter `virtuelenv`

```sh
deactivate # without 's' between e and a
```

## Configuration de l'E. V.

```sh
pip install flask
pip install flask-RESTful
```

> **resource** = quelque chose que notre `API` peut **créer**, **retourner**, etc.

## Code commenté

```python
from flask import Flask
from flask_restful import Resource, Api
# une ressource est une classe qui hérite de Resource

app = Flask(__name__)

api = Api(app)

class Student(Resource):
	# chaque verbe HTTP a sa propre méthode
    def get(self, name):
        return {'student': name}

    #ici on associe la ressource à une route
api.add_resource(Student, '/student/<string:name>')

app.run() # set the port in 5000 per default
```

## `api.add_resoure(Resource,'/route/<string:p>')`

`api` est une collection de ressource associée à une route.