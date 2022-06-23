# 07 L'attribut `key`

Lorsqu'on itére sur un tableau pour générer des composant, React demande un attribut `key` unique pour identifier chaque composant pour les futures modifications.

J'utilise le package `UUID`.

```bash
npm i uuid
```

## `CardList.js`

```jsx
import React from "react";
import Card from "./Card";

import { v4 as uuid } from "uuid";

const CardsList = ({ profiles }) => {
    return (
        <div>
            {profiles.map((profile) => (
                <Card {...profile} key={uuid()} />
            ))}
        </div>
    );
};

export default CardsList;
```

