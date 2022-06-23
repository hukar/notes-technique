# 03 Le module `request`

## Installation

Avec `pip` :

```bash
python3 -m venv venv
source venv/bin/activate

pip install --upgrade pip

pip install requests # attention au 's' final

pip list
Package  Version   
-------- ----------
certifi  2019.11.28
chardet  3.0.4     
idna     2.8       
pip      19.3.1    
requests 2.22.0    
urllib3  1.25.7 
```

## Python Requests

`requests.py`

```python
import requests

def main():
    pass

if __name__ == "__main__":
    main()
```

Permet de lancer la fonction `main` seulement si le fichier est exécuter directement.

Ne lance pas la fonction `main` si le fichier est utilisé en `import`

### Comment ça fonctionne

La variable spéciale `__name__` prends la valeur `"__main__"` si le fichier est exécuté ou bien le nom du fichier (ici par exemple `requests`), si le fichier est importé.

Donc si `__name__` vaut `"__main__"` (le fichier n'est pas importé), exécute la fonction `main()`

## Première requête

```python
import requests

def main():
    response = requests.get("http://www.google.com")
    print("status code: ", response.status_code)

if __name__ == "__main__":
    main()
```

```bash
status code:  200
```

### D'autres attributs de la réponse :

```python
response = requests.get("http://www.google.com")
print("status code: ", response.status_code)

print("Headers: ", response.headers)
print("Content-Type: ", response.headers['Content-Type'])

# print("content: ", response.content)
print("content: ", response.text)
```

## Utilisation d'une `API` public

https://exchangeratesapi.io/

`API` fournissant les taux de changes.

```python
import requests

response = requests.get("https://api.exchangeratesapi.io/latest?base=USD&symbols=GBP")

if response.status_code >= 300:
    print("status code: ", response.status_code)
    raise Exception("There was an error !")
data = response.json() # récupère le JSON

print("JSON data: ", data)
```

```bash
JSON data:  {'rates': {'GBP': 0.7702923977}, 'base': 'USD', 'date': '2020-01-14'}
```

On peut rendre la syntaxe plus élégante :

```python
payload = {"base": "USD", "symbols": "SEK"}
response = requests.get("https://api.exchangeratesapi.io/latest", params=payload)
```

