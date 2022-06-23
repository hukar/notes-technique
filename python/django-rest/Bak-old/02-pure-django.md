## Pure Django API

## On crée un environnement virtuel

```bash
pip install virtualenv
```

```bash
mkdir puredjango
cd puredjango
virtualenv .
```

```bash
(puredjango) kms: puredjango $
```

Si le `virtualenv` n'est pas actif :

```bash
source bin/activate
```

Pour recharger le bash avec `activate`

### Méthode récente

```bash
python3 -m venv venv
```

```bash
source venv/bin/activate
```

## On installe Django

```bash
pip install django # on pourait préciser la version django==1.1, par défaut la dernière 3.0.1

django-admin startproject pdjapi
```

