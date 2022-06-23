
# 05 extension flask-RESTFul pour flask


```python
#list tous les packages installés (par pip ?)
pip3 freeze
```

    astroid==2.1.0
    Click==7.0
    colorama==0.4.1
    Flask==1.0.2
    isort==4.3.4
    itsdangerous==1.1.0
    Jinja2==2.10
    lazy-object-proxy==1.3.1
    MarkupSafe==1.1.0
    mccabe==0.6.1
    pylint==2.2.2
    pyodbc==4.0.25
    six==1.12.0
    Werkzeug==0.14.1
    wrapt==1.11.0


``pip3 freeze`` nous liste les librairies installée et leur version

comment mettre à jour les librairies pour un nouveau projet sans risquer de casser les anciens projets ??


```python
pip install virtualenv
```


### La réponse c'est ``virtualenv``


```python
virtualenv mon_environnement --python=python
```

La syntaxe diffère suivant le système d'exploitation.

```python
# source venv/bin/activate mac or linux
venv/Scripts/activate.bat
```

