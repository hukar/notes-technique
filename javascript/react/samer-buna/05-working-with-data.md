# 05 Working With Data

## Spread properties : `<Card {...user} />` 

On peut utiliser l'opérateur `spread` pour rapidement passer un objet comme propriétés d'un composant :

```jsx
import React, { useState } from "react";
import Card from "./Card";

const testData = [ /* ... */ ];

const CardsList = () => {
    const [data, setData] = useState(testData);

    return (
        <div>
            {data.map(card => <Card {...card}/>)}
        </div>
    );
}

export default CardsList;
```

#### tableau de composants : `{data.map(card => <Card {...card}/>)}` 



## class field

Pour définir le `state` dans une classe, on le fait dans le constructeur :

```jsx
class App extends Component {
    
    constructor(props) {
        super(props);

        this.state = {
            profiles: testData,
        };
    }
```

Avec l'écriture des champs de classe on peut simplifier la syntax :

```jsx
class App extends Component {

    state = {
        profiles: testData,
    }
```

