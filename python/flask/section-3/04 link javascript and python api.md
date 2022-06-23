
# 04 Le lien entre l'API python et javascript


```python
mkdir templates
```

Il faut mettre les templates dans un dossier ``templates``


```python
from flask import Flask, jsonify, render_template

app = Flask(__name__)
```

``render_template`` permet de retourner du html vers le navigateur, le template doit se trouvé dans ``templates``


```python
animals = {
    'animals': [
        {
            'name': 'lynx',
            'power': 120
        },
        {
            'name': 'hiboux',
            'power': 200
        }
    ]
}
```


```python
@app.route('/')
def home():
    return render_template('index.html')
```

ici on renvoie index.html


```python
@app.route('/animals')
def get_animals():
    return jsonify(animals)
```


```python
app.run(port=5026)
```

```python
%%html
<html>
    <head>
       <script>
           xhr = new XMLHttpRequest()
           
           xhr.open('GET','http://127.0.0.1:5026/animals')
           xhr.send()
           
           xhr.onreadystatechange = function() {
                   if(xhr.readyState == 4 && xhr.status == 200) {
                           document.getElementById('output').innerText = xhr.responseText
                   }
           }
        </script>
    </head>
    
    <body>
        <div>
            Hello flask !
            <span id="output"></span>
        </div>
    </body>
</html>
```

Voici le script js de base pour la requête Ajax
