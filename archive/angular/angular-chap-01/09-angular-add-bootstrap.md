## 1 - installer via npm

Vérifier la dernière version :

```bash
npm view bootstrap
```

```bash
...
dist-tags:
latest: 4.1.3
```

Installer la dernière version

```sh
npm install --save bootstrap@latest
```

## 2 - le renseigner dans angular.json

```json
 ...],
            "styles": [
              "node_modules/bootstrap/dist/css/bootstrap.css",  //  <- ici
              "src/styles.css"
            ],
            "scripts": []
          },
...
```

