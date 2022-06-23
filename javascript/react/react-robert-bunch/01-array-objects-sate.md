# 01 modifier le `state` d'un tableau d'objet

```js
changeName = (id, newName) => {
        this.setState({ bots: this.state.bots.map(bot => bot.id === id ? {...bot, name: newName}: bot) });
    };
```

`map` renvoie un nouveau tableau.

`{...bot, name: newName}` renvoie un nouvel objet, c'est équivalent à 

`Object.assign({}, el, { name: newName })`.

```js
const obj = {
    name: "titi",
    age: 56
};

obj; //? { name: 'titi', age: 56 }

const name = "toto";

const toto = Object.assign({}, obj, { name }); // équivalent à Object.assign({}, obj, { name: "toto" })

toto; //? { name: 'toto', age: 56 }
obj; //? { name: 'titi', age: 56 }
```



**<u>Lien overflow :</u>**

https://stackoverflow.com/questions/28121272/whats-the-best-way-to-update-an-object-in-an-array-in-reactjs

## Code complet de mon exemple

```jsx
import React, { Component } from "react";
import { v4 as uuidv4 } from "uuid";

import Bot from "./Bot";

class MasterBot extends Component {
    constructor() {
        super();
        this.state = {
            bots: [
                {
                    id: uuidv4(),
                    name: "T56",
                    power: 1200,
                },
                {
                    id: uuidv4(),
                    name: "DC4TI",
                    power: 800,
                },
                {
                    id: uuidv4(),
                    name: "BOT99",
                    power: 950,
                },
            ],
        };
    }

    changeName = (id, newName) => {
        this.setState({ bots: this.state.bots.map(bot => bot.id === id ? {...bot, name: newName}: bot) });
    };

    changePower = (id, newPower) => {
        this.setState({ bots: this.state.bots.map(bot => bot.id === id ? {...bot, power: newPower} : bot) });
    };

    render() {
        return (<>
            <h1>Bots Party</h1>
            {this.state.bots.map((bot) => (
            <Bot
                changeName={this.changeName}
                changePower={this.changePower}
                bot={bot}
                key={bot.id}
            />
            ))}
        </>);
    }
}

export default MasterBot;
```

