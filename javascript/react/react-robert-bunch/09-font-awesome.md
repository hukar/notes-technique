# 09 Ajouter `Font Awesome`

Documentation officielle :

https://github.com/FortAwesome/react-fontawesome

## 1. `npm install`

```bash
npm i --save @fortawesome/fontawesome-svg-core
npm i --save @fortawesome/free-solid-svg-icons
npm i --save @fortawesome/react-fontawesome
```



## 2. Importer les icônes et la `library`

Cela peut dans le composant parent par exemple

```jsx
import { faDumbbell, faFont, faFileAlt, faDice } from "@fortawesome/free-solid-svg-icons"
import { library } from "@fortawesome/fontawesome-svg-core"
// npm i --save @fortawesome/fontawesome-svg-core
// npm i --save @fortawesome/free-solid-svg-icons
// npm i --save @fortawesome/react-fontawesome

library.add(faDumbbell);
library.add(faFont);
library.add(faFileAlt);
library.add(faDice);

// ...

<ul className="nav nav-pills nav-fill">
    {["dice", "font", "file-alt", "dumbbell"].map(icon => <QuizType icon={icon} />)}
</ul>
```

 

## 3. Utiliser le composant `FontAwesomeIcon`

```jsx
import React from "react";

import {FontAwesomeIcon} from "@fortawesome/react-fontawesome";

function QuizType({ icon }) {
    return (
        <li className="col-sm-3 text-center">
            <div className="nav-card">
                <FontAwesomeIcon icon={icon} size="4x" />
                random
            </div>
        </li>
    );
}

export default QuizType;

```

