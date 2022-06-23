# 02 Un composant simple

```jsx
import React from 'react';
import ReactDom from 'react-dom';

// mon composant écrit en jsx
const App = () => <div>Salut les copains</div>;

reactDom.render(<App/>, document.getElementsByClass('container')[0]);
```

`render` prend en premier argument l'élément `react`  `jsx` et en second argument l'élément parent du `DOM`

### ! c'est `<App/>` et pas `App`

```js
const render: ReactDom.Renderer
(element: React.FunctionComponentElement<any> | React.FunctionComponentElement<any>[], container: Element, callback?: () => void) => void (+6 overloads)
```

