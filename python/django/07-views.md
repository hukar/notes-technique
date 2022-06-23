# views

## `views` basé sur une fonction

```python
from django.http import HttpResponse


def first(request):
    return HttpResponse('hello first')
```

## `views` basé sur une classe

```python
from django.http import HttpResponse
from django.views import View


class Another(View):

    def get(self, request):
        return HttpResponse('Another is a class')
```

Et dans `demo/urls.py` :

```python
from django.urls import path
from .views import Another


urlpatterns = [
    path('another', Another.as_view()),
]
```

Bien sûr on peut utiliser les deux en même temps :

```python
# urls.py
from django.urls import path
from . import views
from .views import Another


urlpatterns = [
    path('first', views.first),
    path('another', Another.as_view()),
]

# views.py
from django.http import HttpResponse
from django.views import View


class Another(View):

    def get(self, request):
        return HttpResponse('Another is a class')


def first(request):
    return HttpResponse('hello first')
```

