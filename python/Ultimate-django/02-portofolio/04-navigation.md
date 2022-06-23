# 04 Navigation

## Réglage du routeur

`urls.py`

```python
from django.contrib import admin
from django.urls import path
from django.conf import settings
from django.conf.urls.static import static
import jobs.views # ici

urlpatterns = [
    path('', jobs.views.home, name='home'), # et la
    path('admin/', admin.site.urls, name='admin'),
] + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
```

Voila ici le moyen d'appeler un contrôleur d'une `app` dans `urls.py`

On donne un nom aux routes pour pouvoir les appeler du template par la suite.

## Appeler une route dans le template

```django
<a class="navbar-brand" href="{% url 'home' %}">Hukar-Job</a>
```

### `{% url 'home' %}`

## Passer des paramètres

```django
{% for article in blogs.all %}
        <div>
            <a href="{% url 'detail' article.id %}"><h3 class="mb-2"
            >{{ article.title }}</h3></a>
```

### `{% url 'detail' article.id %}`

#### `{% url 'nom_route' param1 param2 %}`