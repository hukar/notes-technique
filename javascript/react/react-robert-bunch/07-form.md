# 07 Formulaires

Pour qu'un formulaire soit sous le contrôle de **React** il faut :

1. Que la valeur `value` de l'élément de formulaire soit liée à `this.state.myField`
2. Que les changement de valeur déclenche un `setState`

```jsx
/* eslint-disable jsx-a11y/accessible-emoji */
import React, { Component } from "react";

class FormPractice extends Component {
    constructor() {
        super();

        this.state = {
            name: "",
            description: "",
            fruit: "something wrong",
        };
    }

    handleSubmit = (ev) => {
        ev.preventDefault();
        console.log(this.state);
    };

    changeName = (ev) => {
        this.setState({ name: ev.target.value });
    };
    changeDescription = (ev) => {
        this.setState({ description: ev.target.value });
    };
    changeFruit = (ev) => {
        this.setState({ fruit: ev.target.value });
    };

    render() {
        return (
            <form onSubmit={this.handleSubmit}>
                <input
                    type="text"
                    placeholder="Enter your name"
                    value={this.state.name}
                    onChange={this.changeName}
                    />

                <textarea
                    value={this.state.description}
                    onChange={this.changeDescription}
                    />


                <select
                    value={this.state.fruit}
                    onChange={this.changeFruit}
                    >
                    <option value="apple">apple 🍎</option>
                    <option value="banana">banana 🍌</option>
                    <option value="pinapple">pinapple 🍍</option>
                    <option value="grap">grap 🍇</option>
                </select>

                <input type="submit" value="submit" />
            </form>

        );
    }
}

export default FormPractice;
```

Pour `textarea` et `select` ce sont des formes particulières à **React**.

