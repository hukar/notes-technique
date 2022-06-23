# 02 Formulaire avec `django`

## Apeller une autre page avec `action`

### `<form action="{% url 'count' %}">`

dans un template `home.html`

```django
<form action="{% url 'count' %}">
  <textarea id="" cols="30" rows="10" name="fulltext"></textarea>
  <br />
  <input type="submit" value="Submit !!">
</form>
```

`{% url 'nom_url' %}` permet d'écrire une `url` grâce à son nom définie dans `urls.py`

#### ! Il faut des guillemets autour du nom `'nom_url'`

Dans `urls.py`

```python
from django.urls import path
from . import views

urlpatterns = [
    path('count/', views.count, name='count'),
]
```

### `path('count/', views.count, name='count'),`

On a l'attribut `name='nom_url'`

Ce nom est indépendant de la syntaxe de l'url réel :

```python
urlpatterns = [
    path('themagecount/', views.count, name='count'),
]
```

Fonctionnerait parfaitement.

## Récupérer les données de formulaire

###  `fulltext = request.GET['fulltext']`

Dans `views.py`

```python
from django.http import HttpResponse
from django.shortcuts import render


def home(request):
    return render(request, 'home.html', {'title': 'Word Count'})


def count(request):
    fulltext = request.GET['fulltext']
    return render(request, 'count.html', {'text': fulltext})
```