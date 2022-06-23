# 07 Les méthodes dans les modèles

Si on veut transformer une donnée, on peut créer à l'intérieur de son modèle une méthode :

```python
from django.db import models

# Create your models here.
class Blog(models.Model):
    title = models.CharField(max_length=155)
    pu_date = models.DateTimeField(auto_now_add=True)
    body = models.TextField()
    image = models.ImageField(upload_to='images')

    def __str__(self):
        return self.title

    def summary(self):
        summary = self.body.split()[0:15] + ["..."]
        return " ".join(summary)

    def pub_date_pretty(self):
        return self.pu_date.strftime('%e %b %Y')

```

La première méthode `__str__` permet seulement d'améliorer l'affichage dans l'interface d'administration.

en effet l'interface utilise cette méthode pour nommer les enregistrements :

On passe d'un affichage comme ça ->

![Screenshot 2020-01-13 at 12.43.43](assets/Screenshot 2020-01-13 at 12.43.43.png)

À un affichage comme ceci ->

![Screenshot 2020-01-13 at 12.44.01](assets/Screenshot 2020-01-13 at 12.44.01.png)

La deuxième méthode permet de générer un résumé

La troisième méthode utilise `strftime` pour améliorer l'affichage de la date

Belle documentation internet : https://strftime.org/

## Utilisation dans le `template`

On appelle une méthode de la même manière qu'on appelle un attribut :

```django
<h5 class="text-muted">{{ article.pub_date_pretty }}</h5>
            
<p>{{ article.summary }}</p>
```

