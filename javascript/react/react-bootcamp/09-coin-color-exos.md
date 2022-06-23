# 09 Exercices

## La pièce

### `Coin.js`

```jsx
import React, { Component } from "react";
import "./Coin.css";

export default class Coin extends Component {
    render() {
        return (
            <div className="Coin">
                <i className={`fas fa-${this.props.face}`}></i>
            </div>
        );
    }
}
```

### `FlipCoin.js`

```jsx
import React, { Component } from "react";
import Coin from "./Coin";

export default class FlipCoin extends Component {
    constructor(props) {
        super(props);
        this.state = {
            face: "grimace",
            nbFlip: 0,
            nbHead: 0,
            nbTail: 0
        };
        this.handleClick = this.handleClick.bind(this);
    }

    handleClick() {
        this.setState(prevState => {
            const coin = Math.floor(Math.random() * 2);
            console.log(prevState);
            return {
                face: coin === 1 ? "grimace" : "circle",
                nbFlip: prevState.nbFlip + 1,
                nbHead: coin === 1 ? prevState.nbHead + 1 : prevState.nbHead,
                nbTail: coin === 1 ? prevState.nbTail : prevState.nbTail + 1
            };
        });
    }

    render() {
        return (
            <div>
                <Coin face={this.state.face} />
                <p>
                    <button onClick={this.handleClick}>Flip Coin</button>
                </p>
                <p>{`flip : ${this.state.nbFlip} Head: ${this.state.nbHead} Tail: ${this.state.nbTail}`}</p>
            </div>
        );
    }
}
```

Erreur à cause de l'opérateur `++`

```jsx
// fonctionne
return {
  face: coin === 1 ? "grimace" : "circle",
  nbFlip: prevState.nbFlip + 1,
  nbHead: coin === 1 ? prevState.nbHead + 1 : prevState.nbHead,
  nbTail: coin === 1 ? prevState.nbTail : prevState.nbTail + 1
};

// n'est pas équivalent à
// ne fonctionne pas :
return {
  face: coin === 1 ? "grimace" : "circle",
  nbFlip: prevState.nbFlip++,
  nbHead: coin === 1 ? prevState.nbHead++ : prevState.nbHead,
  nbTail: coin === 1 ? prevState.nbTail : prevState.nbTail++
};
```

### Amélioration

sortir la logique dans une fonction et dans un fichier helpers

Utiliser `defaultprop` pour réunir les états possible de la pièce

Ré-écrire les ternaires en les simplifiant

Ne pas mettre de logique dans `handleClick` mais appeler une autre méthode `flipCoin`

`helpers.js`

```js
function choice(arr) {
    const ind = Math.floor(Math.random() * arr.length);
    return arr[ind];
}

export { choice };
```

`FlipCoin.js`

```jsx
import React, { Component } from "react";
import Coin from "./Coin";
import { choice } from "./helpers";

export default class FlipCoin extends Component {
    static defaultProps = {
        faces: ["grimace", "circle"]
    };
    constructor(props) {
        super(props);
        this.state = {
            face: null,
            nbFlip: 0,
            nbHead: 0,
            nbTail: 0
        };
        this.handleClick = this.handleClick.bind(this);
    }

    flipCoin() {
        const newFace = choice(this.props.faces);

        this.setState(prevState => {
            return {
                face: newFace,
                nbFlip: prevState.nbFlip + 1,
                nbHead: prevState.nbHead + (newFace === "grimace" ? 1 : 0),
                nbTail: prevState.nbTail + (newFace === "circle" ? 1 : 0)
            };
        });
    }

    handleClick() {
        this.flipCoin();
    }

    render() {
        return (
            <div>
                <Coin face={this.state.face} />
                <p>
                    <button onClick={this.handleClick}>Flip Coin</button>
                </p>
                <p>{`flip : ${this.state.nbFlip} Head: ${this.state.nbHead} Tail: ${this.state.nbTail}`}</p>
            </div>
        );
    }
}
```

## Color Boxes

`Helpers.js`

```js
function choice(arr) {
    const ind = Math.floor(Math.random() * arr.length);
    return arr[ind];
}

function generate(arr, nb) {
    return [...Array(nb)].map(v => choice(arr));
}

export { choice, generate };

```



`ColorGrid.js`

```jsx
import React, { Component } from "react";
import "./ColorGrid.css";
import { generate } from "./helpers";
import Box from "./Box";

class ColorGrid extends Component {
    static defaultProps = {
        colors: [ "deeppink", "limegreen", "mediumvioletred", ... ]
    };

    constructor(props) {
        super(props);
        this.state = {
            boxes: generate(this.props.colors, 20)
        };

        this.handleClick = this.handleClick.bind(this);
    }

    changeBoxColor(ind) {
        this.setState(prevState => {
            const newColors = this.props.colors.filter(
                color => color !== prevState.boxes[ind]
            );

            const newBoxes = [...prevState.boxes];
            newBoxes[ind] = generate(newColors, 1)[0];
            return { boxes: newBoxes };
        });
    }

    handleClick(ind) {
        this.changeBoxColor(ind);
    }

    render() {
        return (
            <div className="ColorGrid">
                {this.state.boxes.map((color, ind) => (
                    <div onClick={() => this.handleClick(ind)} key={ind}>
                        <Box color={color} key={ind} />
                    </div>
                ))}
            </div>
        );
    }
}

export default ColorGrid;
```

`Box.js`

```jsx
import React, { Component } from "react";
import "./Box.css";

class Box extends Component {
    render() {
        return (
            <div
                className="Box"
                style={{ backgroundColor: this.props.color }}
            ></div>
        );
    }
}

export default Box;
```

### css intéressants :

```css
/* pour le composant parent */
.ColorGrid {
    display: flex;
    flex-wrap: wrap; /* passe à la ligne suivante */
}

```

