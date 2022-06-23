# 01 StartUp

Création de `main.py` et `requirements.txt`.

`requirements.txt`

```txt
fastapi
```
### `pip install -r requirements.txt`

```bash
(venv) 🧨 hukar:chap-03$ pip install -r requirements.txt
Collecting fastapi
  Downloading fastapi-0.63.0-py3-none-any.whl (50 kB)
     |████████████████████████████████| 50 kB 3.6 MB/s 
Collecting starlette==0.13.6
  Downloading starlette-0.13.6-py3-none-any.whl (59 kB)
     |████████████████████████████████| 59 kB 8.9 MB/s 
Collecting pydantic<2.0.0,>=1.0.0
  Downloading pydantic-1.8-cp39-cp39-macosx_10_9_x86_64.whl (2.7 MB)
     |████████████████████████████████| 2.7 MB 9.6 MB/s 
Collecting typing-extensions>=3.7.4.3
  Downloading typing_extensions-3.7.4.3-py3-none-any.whl (22 kB)
Installing collected packages: typing-extensions, starlette, pydantic, fastapi
Successfully installed fastapi-0.63.0 pydantic-1.8 starlette-0.13.6 typing-extensions-3.7.4.3
```

On voit que `starlette`, `pydantic` et `typing-extensions` sont aussi installés.



## `nodemon`

`nodemon` est une façon de relancer `main.py` efficacement.