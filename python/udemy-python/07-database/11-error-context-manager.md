# Gestion des erreurs avec le context manager

## Dans la classe de connection

```python
#leave from the context manager
    def __exit__(self, exc_type, exc_val, exc_tb):
        # exception type exc_type None value per default
        # exception value exc_value
        # exception traceback exc_tb (= traçabilité, traçage)
        # if nothing to commit sqlite do nothing it's fine
        if exc_type or exc_val or exc_tb:
            print('close connection')
            self.connection.close()
        else:
            self.connection.commit()
            self.connection.close()
```

Les trois derniers arguments sont les erreurs possible, leur valeur par default est `None`.

`None` est considéré comme `False` dans un test.

