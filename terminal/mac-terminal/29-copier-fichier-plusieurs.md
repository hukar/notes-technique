# 29 Copier un fichier dans plusieurs fichiers

```bash
tee < MakesController.cs {Model,Vehicle,Customer}Controller.cs >/dev/null
```

ou bien 

```bash
for f in {Model,Vehicle,Customer}Controller.cs; do cp MakesController.cs $f; done
```

