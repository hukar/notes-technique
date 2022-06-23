# 03 PostgreSQL

## 1. Télécharger sur le site la version `postgres.app` pour mac![Screenshot 2020-01-08 at 15.36.58](assets/Screenshot 2020-01-08 at 15.36.58.png)

à l'adresse : https://www.postgresql.org/download/macosx/

## 2. Lancer l'application

Double click sur la base `postgres`

Cela ouvre une invite de commande

### Lister les utilisateurs

```bash
postgres=# \du
                                   List of roles
 Role name |                         Attributes                         | Member of 
-----------+------------------------------------------------------------+-----------
 kms       | Superuser, Create role, Create DB                          | {}
 postgres  | Superuser, Create role, Create DB, Replication, Bypass RLS | {}

```

On va utiliser le profil `postgres`

### Ajouter un mot de passe

```bash
postgres=# \password postgres
Enter new password: 
Enter it again:
```

### Créer une base de données

```bash
postgres=# CREATE DATABASE portfoliodb;
CREATE DATABASE
```

## 3. Modifier le `settings.py`

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
```

## 4. Installer `psycopg2-binary`

```bash
pip install psycopg2-binary
```

## 5. `Migrate` + `createsuperuser`

```bash
./manage.py migrate

./manage.py createsuperuser
```

Lorsqu'on change de DB on doit recréer un super user.

#### ! les images de l'ancienne DB reste accessible via leurs urls

## 6. Récuperer les données `dump data`

### D'abord un `dump`

```bash
# un dump de toute la table
(venv) kms: portofolio-project $ ./manage.py dumpdata > db.json

# un dump de la table jobs
(venv) kms: portofolio-project $ ./manage.py dumpdata jobs > db2.json

# un dump lisible (pas en une ligne très longue)
(venv) kms: portofolio-project $ ./manage.py dumpdata jobs --indent 4 > db3.json

```

### Ensuite un `load`

Pour réenregistrer les données dans la nouvelle base :

```bash
./manage.py loaddata db3.json
```

