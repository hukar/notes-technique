# 16 renommer plusieurs fichiers

Avec un boucle `for` :

```bash
for f in *_php.bin;
do mv "$f" "_$f";
done;
```

