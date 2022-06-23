# Recherche d'un seul enregistrement

## Dans le routeur `urls.py`

On ajoute un paramètre à l'`url`

```python
from django.urls import path

from . import views

urlpatterns = [
    path('', views.allblogs, name="allblogs"),
    path('<int:blog_id>/', views.detail, name="detail"),
]
```

`<type:nom_param>/` permet d'envoyer un paramètre d'`url` au contrôleur dans `views.py`

## Dans le contrôleur `views.py`

```python
from django.shortcuts import render, get_object_or_404

from .models import Blog


def detail(request, blog_id):
    detailblog = get_object_or_404(Blog, pk=blog_id)
    return render(request, 'blog/detail.html', {'detail': detailblog})
```

On import `get_object_or_404`

Endeuxième paramètre on récupère la valeur passée par l'`url`

`detailblog = get_object_or_404(Blog, pk=blog_id)` permet de récupérer l'enregistrement ou d'envoyer vers la page `404`, on lui passe l'`id` reçu en paramètre