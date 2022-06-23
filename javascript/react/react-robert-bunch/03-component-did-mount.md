# 03 `ComponentDidMount`

Cette fonction est appelée une fois le composant monté.

#### Elle est appelée qu'une seule fois dans la vie du composant, une fois que celui-si est monté dans le `DOM`.

C'est l'endroit où mettre les appelles `AJAX`.

```js
import React, { Component } from "react";
import "./App.css";
import axios from "axios";

class App extends Component {
    constructor() {
        super();

        this.state = {
            temp: 0,
        };
    }

    componentDidMount() {
        const url =
            "https://api.openweathermap.org/data/2.5/weather?q=Brussels&units=metric&appid=e312dbeb8840e51f92334498a261ca1d";

        axios.get(url).then((resp) => {
            this.setState({ temp: resp.data.main.temp });
        });
    }

    render() {
        return (
            <div className="App">
                <h1>{this.state.temp}</h1>
            </div>
        );
    }
}

export default App;
```

## Utilisation: quand on a besoin du DOM

Si on utilise **Materialize** et qu'on place du javascript manipulant le `DOM`, on doit le mettre dans componentDidMount, car avant le `DOM` n'est pas disponnible :

```js
componentDidMount() {
        const url =
            "https://api.openweathermap.org/data/2.5/weather?q=Brussels&units=metric&appid=e312dbeb8840e51f92334498a261ca1d";

        axios.get(url).then((resp) => {
            this.setState({ temp: resp.data.main.temp });
        });

        var elems = document.querySelectorAll(".modal");
        window.M.Modal.init(elems);
    }
```

**Remarque :** On doit rajouter `window` devant `M.Modal.init(elems)`.

Pour intéragir avec le navigateur on utilise `componentDidMount`.

