# Nombre aléatoire

Un nombre aléatoire est généré en appuyant sur un bouton, si le nombre est égal à 7, on gagne.

```jsx
import React, { useState } from "react";

export default function NumGame() {
  const [num, setNum] = useState(1);

  const handleNum = e => {
    const random = Math.floor(Math.random() * 10 + 1);
    setNum(random);
  };

  return (
    <div>
      <h3>{num}</h3>
      <p>
        {num === 7 ? (
          <span style={{ fontSize: "24px" }}>YOU WIN {"!".repeat(5)}</span>
        ) : (
          <button onClick={handleNum}>random number</button>
        )}
      </p>
    </div>
  );
}
```

Le **ternaire** dans le template est le plus simple moyen de manipuler un affichage conditionnel.