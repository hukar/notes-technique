# 01 mise à jour en vuejs

## Petit exemple

### Le `html :`

```html
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <meta http-equiv="X-UA-Compatible" content="ie=edge" />
        <title>Cookies Banner</title>
        <link rel="stylesheet" href="cookie.css" />
    </head>
    <body>
        <div id="app" @mousemove="onMouseMove">
            {{message}}
            <div :style="styleOne"></div>
            <div :style="styleTwo"></div>
        </div>

        <script src="vue.js"></script>
        <script src="cookie.js"></script>
    </body>
</html>

```

Je récupère `vue.js` en le copiant depuis un `CDN :https://cdn.jsdelivr.net/npm/vue/dist/vue.js `

`{{ var }}` : affiche une variable

`@mousemove` : attache un écouteur d'événement

`:style` attache l'attribut `style`

### Le fichier `cookie.js`

```js
function transform(offset) {
    const cos = Math.cos(offset);
    const sin = Math.sin(offset);
    return {
        transform: `matrix3d(${sin}, ${-cos}, ${sin}, 0, ${-cos}, ${sin}, 0, 0, 0, ${cos}, ${cos}, ${sin}, 0, 0, 0, 1)`
    };
}

var app = new Vue({
    el: "#app",
    data: {
        message: "hello vue !",
        styleOne: {},
        styleTwo: {}
    },
    methods: {
        onMouseMove(event) {
            this.styleOne = transform(-event.clientX / event.clientY);
            this.styleTwo = transform(event.clientX / event.clientY);
        }
    }
});
```

On crée une instance de `Vue`

### Le `css`

```css
#app {
    height: 100vh;
    width: 100vw;
}

#app div {
    position: absolute;
    height: 100%;
    width: 100%;
    box-shadow: 0 0 50px grey;
    background-image: url("./background.jpg");
}
```

