# 04 l'`admin` de `Django`

## URL

```bash
localhost:8000/admin
```

![Screenshot 2020-01-01 at 17.30.05](assets/Screenshot 2020-01-01 at 17.30.05.png)

## Création d'un administrateur

```bash
python manage.py createsuperuser
```

## Ajout du modèle dans `Django administration`

Pour l'instant on ne peut pas gérer les `Book` dans l'interface d'admin.

![Screenshot 2020-01-01 at 17.39.14](assets/Screenshot 2020-01-01 at 17.39.14.png)

Il faut ajouter le modèle au fichier `admin.py`

```python
from django.contrib import admin
from demo.models import Book


admin.site.register(Book)
```

![Screenshot 2020-01-01 at 17.47.24](assets/Screenshot 2020-01-01 at 17.47.24.png)

Et voilà le modèle `Book` apparaît

## Customisation de l'interface d'admin

`models.py`

```python
class Book(models.Model):
    title = models.CharField(name="title", max_length=55, unique=True)
    description = models.TextField(max_length=255, blank=True)
    created = models.DateTimeField(auto_now_add=True,)
    size = models.IntegerField()
```

`admin.py`

```python
from django.contrib import admin
from django.contrib.auth.models import Group
from demo.models import Book

admin.site.site_header = 'Admin Book Test'


class BookAdmin(admin.ModelAdmin):
    list_display = ('title', 'description', 'created', 'size')
    list_filter = ('created', 'size')

    # exclude = ('title',)
    # fields = ('title',)


admin.site.register(Book, BookAdmin)
admin.site.unregister(Group)
```

`admin.site.site_header` customise le header de la page d'aministration

`admin.site.unregister` permet de retirer la gestion des groupes

`exclude` et `fields` permettent de manière soustractive ou additive de n'afficher que certains champs

`list_display` certainement le plus utile, permet d'afficher des champs dans la vue en liste

`list_filter` permet d'ajouter des filtres

## Avant / Après

![Screenshot 2020-01-03 at 14.59.00](assets/Screenshot 2020-01-03 at 14.59.00.png)

![Screenshot 2020-01-03 at 14.59.13](assets/Screenshot 2020-01-03 at 14.59.13.png)

![Screenshot 2020-01-03 at 14.59.50](assets/Screenshot 2020-01-03 at 14.59.50.png)

![Screenshot 2020-01-03 at 15.00.36](assets/Screenshot 2020-01-03 at 15.00.36.png)