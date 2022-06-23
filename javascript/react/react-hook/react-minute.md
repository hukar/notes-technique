# Ajouter `React` en une minute sur un site

Dans une page `html` déjà existante

## 1 on ajoute les librairies

```jsx
<script src="https://unpkg.com/react@16/umd/react.development.js" crossorigin></script>
<script src="https://unpkg.com/react-dom@16/umd/react-dom.development.js" crossorigin></script>
```

## 2 on ajoute un conteneur `DOM`

```html
<div id="my-container"></div>
```

## 3 on lie son `script` de composant

```html
  <script
          src="https://unpkg.com/react@16/umd/react.development.js"
          crossorigin
          ></script>
  <script
          src="https://unpkg.com/react-dom@16/umd/react-dom.development.js"
          crossorigin
          ></script>

  <script src="app.js"></script>
</body>
```

## 4 On écrit le composant `React`

```js
"use strict";

const e = React.createElement;

class Button extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            msg: "hello les copains"
        };
    }

    render() {
        return e(
            "button",
            {
                onClick: () => this.setState({ msg: "click click" }),
                type: "button"
            },
            this.state.msg
        );
    }
}

const domContainer = document.getElementById("my-container");

ReactDOM.render(e(Button), domContainer);
```

### Utiliser Babel pour les rendus complexes

On peut avoir un html compliqué en `jsx`:

```jsx
render() {
        return (<div class={this.state.customClass}>

            <p>
                {this.props.someText}
            </p>

            <button onclick={this.doSomething}>click me !!</button>
        </div>);
    }
```

En passant par `babeljs.io` on récupère la version sans `JSX`:

```js
render() {
        return React.createElement(
            "div",
            {
                className: this.state.customClass
            },
            React.createElement("p", null, this.props.someText),
            React.createElement(
                "button",
                {
                    onClick: this.doSomething
                },
                "click me !!"
            )
        );
    }
```

Cela peut-être intéressant pour modifier des parties d'un site existant sans `transpiler` en live via un script `Babel`

## L'appel de script minifier

```html
<script src="https://unpkg.com/react@16/umd/react.production.min.js" crossorigin></script>
<script src="https://unpkg.com/react-dom@16/umd/react-dom.production.min.js" crossorigin></script>
```

Il faudrait aussi minifier son propre javascript.