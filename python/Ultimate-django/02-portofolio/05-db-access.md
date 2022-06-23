# Accès en base de données

## 1. Le modèle

`models.py`

```python
from django.db import models


class Job(models.Model):
    image = models.ImageField(upload_to='images/')
    summary = models.CharField(max_length=200)
```

## 2. le contrôleur

`views.py`

```python
from django.shortcuts import render

from .models import Job


def home(request):
    jobs = Job.objects
    return render(request, 'jobs/home.html', {'jobs': jobs})
```

La simple ligne `jobs = Job.objects` opère la magie et connecte `django` à la `DB`

## 3. La vue

`home.html`

```django
<div class="row">
  {% for job in jobs.all %}
  <div class="col-md-4">
    <div class="card mb-4 shadow-sm">
      <img class="card-img-top" src="{{ job.image.url }}">
      <div class="card-body">
        <p class="card-text">{{ job.summary }}</p>
      </div>
    </div>
  </div>
  {% endfor %}
</div>
```

Pour itérer sur tous les jobs : `jobs.all`

Pour avoir l'adresse de l'image : `job.image.url`

## 4. Dans la configuration

`settings.py`

```python
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql',
        'NAME': 'portfoliodb',
        'USER': 'postgres',
        'PASSWORD': 'root',
        'HOST': 'localhost',
        'PORT': '5432',
    }
}

# et plus loin pour l'url des images

# Static files (CSS, JavaScript, Images)
# https://docs.djangoproject.com/en/3.0/howto/static-files/

STATIC_URL = '/static/'

MEDIA_ROOT = os.path.join(BASE_DIR, 'media')

MEDIA_URL = '/media/'
```

