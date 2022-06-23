## UUID library

Pour avoir des clés unique Universally Unique IDentifier

```bash
npm install uuid
```

Utilisation :

```jsx
import uuid from "uuid/v4";

class ShoppingListForm extends Component {
  constructor(props) {
    super(props);

    this.state = {
      name: "",
      qty: "",
      id: uuid()
    };

// ...
```

