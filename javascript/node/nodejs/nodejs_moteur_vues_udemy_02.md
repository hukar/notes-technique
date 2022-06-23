# view engine

## express ejs

`npm install ejs`

On configure notre app.js :

```js
// on présise un répertoire contenant les vues
app.set('views', 'views')
// on choisi un view engine autre : handlebars jade
app.set('view engine', 'ejs')
```

Définir un dossier static où mettre les css par exemple

`app.use` défini un middleware

```js
// le dossier s'appelle styles, 
// l'adresse pour le joindre est /public
app.use('/public', express.static('styles'))
```

on crée un ficier **index.ejs**

```html
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <!-- on utilise /public qui pointe en réalité sur /styles -->
    <link rel="stylesheet" href="/public/mon-style.css">
    <title>view engine EJS</title>
</head>
<body>
    <h3>express - nodejs</h3>
   <p>je suis un template EJS</p> 
</body>
</html>
```