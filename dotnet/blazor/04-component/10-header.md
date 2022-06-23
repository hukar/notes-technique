# 10 `header`

On peut passer des données au `header html` grâce aux composant suivant :

## `<PageTitle>`

Permet de définir le titre de la page :

```cs
@page "/"

<PageTitle>Gros Caca 💩</PageTitle>

<h1>Hello, world!</h1>
```

<img src="assets/title-setter-gos-caca.png" alt="title-setter-gos-caca" style="zoom:50%;" />



## `<HeadContent>`

Permet d'ajouter des composant au `<head>` depuis une page `razor`.

```cs
@page "/"

<PageTitle>Gros Caca 💩</PageTitle>
<HeadContent>
    <meta lang="be-fr" />
</HeadContent>
```

<img src="assets/adding-head-content-meta-for-exemple.png" alt="adding-head-content-meta-for-exemple" style="zoom:50%;" />



