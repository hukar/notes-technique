# `UUID` Unirversal Unique IDentifier

Unicité très probable (car pas de coordination centrale).

```bash
npm install uuid
```

## Utilisation dans `React`

```jsx
import uuid from "uuid/v4";

// ...

{users.map(user => (
  <Name style={{ color: user.color }} key={uuid()}>
    {user.name}
  </Name>
))}
```

Cela permet d'avoir facilement une valeur unique de `key`