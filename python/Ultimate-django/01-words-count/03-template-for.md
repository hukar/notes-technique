# 03 La boucle `for` dans un template

## exemple avec un système de comptage de mots

### `{% for word, wordcount in words %}`

```django
<ul>
  {# for word, wordcount in words.items #}
  {% for word, wordcount in words %}
  <li>{{ word }} : {{ wordcount }}</li>
  {% endfor %}
</ul>
```

`words.items` est a utiliser si on reçoit un dictionnaire

Ici le dictionnaire a été transformé dans `views.py`:

```python
from django.http import HttpResponse
from django.shortcuts import render
import re


# ici une méthode plus précise pour compter les mots
def count_word(text):
    pattern = '[A-zÀ-ú^-]+'
    result = re.findall(pattern, text)

    return len(result)


def count(request):
    fulltext = request.GET['fulltext']
    
    # transformer la chaîne de caractère en liste de mots
    wordlist = fulltext.split()

    # dictionnaire dans lequel on va compter les mots
    worddictionnary = {}

    # comptage des mots
    for word in wordlist:
        if word in worddictionnary:
            worddictionnary[word] += 1
        else:
            worddictionnary[word] = 1

    # la fonction pour key dans sorted pour trier sur le deuxième élément
    def second(elem):
        return elem[1]

    # trie des mots
    words = sorted(worddictionnary.items(), key=second, reverse=True)

    return render(
                    request,
                    'count.html',
                    {
                        'fulltext': fulltext,
                        'count': len(wordlist),
                        'words': words
                    },
                  )
```



## Flow pour une page `django`

### 1 Définir une `url` -> `urls.py`

```python
from django.urls import path
from . import views

urlpatterns = [
    path('about/', views.about, name='about'),
]
```



### 2 Créer une fonction (un contrôleur) -> `views.py`

```python
from django.shortcuts import render

def about(request):
    return render(request, 'about.html', {'title': 'about'})
```



### 3 Créer un template (une vue) -> `templates/mavue.html`

```django
<head>
    <meta charset="UTF-8">
    <title>{{ title }}</title>
</head>
<body>
    <h1>Lorem ipsum dolor sit amet, consectetur.</h1>
    <p>
        Lorem ipsum dolor sit amet, consectetur adipisicing elit. Ad adipisci aliquid aspernatur ex fugiat illo impedit, inventore iste iusto vitae. Dolores odit optio reiciendis saepe! Corporis ducimus ea eius enim est expedita harum inventore labore magni maxime minima nam nihil officia provident quas quasi quibusdam quidem quos, reiciendis velit?
    </p>
</body>
</html>
```

### `{% now 'Y' %}`

Affiche l'année en cours 