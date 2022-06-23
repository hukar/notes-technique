
# écrire dans un fichier


```python
fichier = open("toto.tx","w")
print(fichier) 
```

    <_io.TextIOWrapper name='toto.tx' mode='w' encoding='cp1252'>
    

Pour créer un fichier : open("monfichier.txt","x") erreur si le fichier existe déjà. 
- x création (erreur)
- r read
- w write (+ création si besoin)
- a mode ajout
- b binaire
- t text


```python
fichier.close()
print(fichier)
```

    <_io.TextIOWrapper name='toto.tx' mode='w' encoding='cp1252'>
    

## écrire dans un fichier


```python
f = open("data.txt", "w")
f.write("Salut copain\n")
f.close()

f = open("data.txt", "a")
f.write("hello kitty")
f.close()
```

## Lire le contenu


```python
f = open("data.txt","r")
print(f.read())
f.close()
```

    Salut copain
    hello kitty
    

## correction


```python
import os

cur_dir = os.path.dirname(__file__)
print(cur_dir)
print(__file__)
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    <ipython-input-17-6cdf0352f919> in <module>
          1 import os
          2 
    ----> 3 cur_dir = os.path.dirname(__file__)
          4 print(cur_dir)
          5 print(__file__)
    

    NameError: name '__file__' is not defined


Cela ne fonctionne pas dans jupyter notebook.  
Alternative :


```python
%pwd
```




    'C:\\Users\\kms\\Google Drive\\note-technique\\python'




```python
from pathlib import Path

Path().resolve()
```




    WindowsPath('C:/Users/kms/Google Drive/note-technique/python')




```python
with open("fich.txt","w") as f:
    f.write("bonjour les copains !")
    
# équivalent de 
f = open("fich.txt","w")
f.write("bonjour les lapins ...")
f.close()

with open("fich.txt","r") as f:
    print(f.read())
```

    bonjour les lapins ...
    

#### avec `with` pas besoin de close() = écriture plus élégante
