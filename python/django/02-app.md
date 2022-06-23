# 02 Les `app`

## Créer une `app`

Un projet `Django` est organisé avec des `app`

Chaque `app` ayant une responsabilité

Deux façon de créer une `app`

```bash
(venv) kms: django-react $ django-admin startapp  demo
(venv) kms: django-react $ python manage.py startapp demo2
```

## Anatomie

![Screenshot 2019-12-31 at 16.49.23](assets/Screenshot 2019-12-31 at 16.49.23.png)

`models.py` : Ici on créera des objets représentant les données à mettre en Base de Données.

On utilisera les `migrations` pour mettre à jour la base de données.

`views.py` sera une sorte d'`end point` pour notre `API`