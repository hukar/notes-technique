# Class Component

Es6 class

Props -> class [state] -> jsx

`Component/welcome.js`

```jsx
import React, { Component } from 'react' # assignation par décomposition

class Welcome extends Component {
    render() {
        return <h2>Welcome everybody</h2>
    }
}

export default Welcome
```

L'objet `react` est affecté à l'objet `React` et l'objet `react.Component` à l'objet `Component`.

C'est une assignation par décomposition (pattren matching).

`App.js`

```jsx
<Welcome />
```

Comme il n'y a pas de contexte, on utilise une balise auto-fermante.