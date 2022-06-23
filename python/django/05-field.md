# 05 Field

## Field options

```python
from django.db import models

# Create your models here.
class Book(models.Model):
    title = models.CharField(max_length=36)
    title = models.CharField(null=True) # le champ peut être null
    title = models.CharField(blank=True) # le champ peut être vide (blanc)
    title = models.CharField(blank=False) # le champ est requis = required
    title = models.CharField(blank=False, unique=True) # la valeur du champ est unique dans la table
    title = models.CharField(default='valeur par défaut') # ...
    BOOKS = (
    	('BH','Bilbot the Hobbit'),
        ('LOTR', 'Lord Of The Ring'),
    )
    title = models.CharField(choice=BOOKS) # limiter le choix à quelques valeurs
```

## Notre modèle

```python
from django.db import models

class Book(models.Model):
    title = models.CharField(max_length=36, blank=False, unique=True)
```

Voila notre modèle réel

On doit construire une `migration`:

```bash
./manage.py makemigrations
```

On obtient un nouveau fichier dans le dossier migrations :

`demo/migrations/0002_auto_20200101_1813.py`

```python
from django.db import migrations, models

class Migration(migrations.Migration):

    dependencies = [
        ('demo', '0001_initial'),
    ]

    operations = [
        migrations.AlterField(
            model_name='book',
            name='title',
            field=models.CharField(max_length=36, unique=True),
        ),
    ]
```

Maintenant il faut rendre les changements efféctif dans la base de données :

```bash
./manage.py migrate
```

## Field type

```python
from django.db import models


class Book(models.Model):
    title = models.CharField(name="title", max_length=55, unique=True)
    description = models.TextField(max_length=255, blank=True)
    created = models.DateTimeField(auto_now_add=True,)  # only set at the creation
    modified = models.DateTimeField(auto_now=True,)  # set when the book is modified
    published = models.DateField(blank=True, null=True)
    # on a aussi models.TimeField
    is_published = models.BooleanField(default=False,)
    price = models.DecimalField(default=0, max_digits=6, decimal_places=2)
    size = models.IntegerField()
    cover = models.FileField(upload_to='covers/')
    cover = models.ImageField(blank=True, upload_to='covers/')

```

`TextField` sert à enregistrer plutôt des textes et `CharField` plutôt de courtes phrases.

`auto_now_add=True` enregistre la date à la création

`auto_now` enregistre la date à chaque modification

`DecimalField` permet d'éviter les bizareries du type `float` :

 `0.2 + 0.1 = 0.30000000000000004`

`ImageField `  nécéssite d'installer `Pillow`

```bash
pip install Pillow
```

`Pillow` est une librairie de manipulation d'image.

`upload_to` permet de définir un fichier pour les téléchargements