# 10 utilisation d'un spinner  `font awesome`

Mettre une propriété `ready` dans le `state` à `false`.

Lorsque la requête `AJAX` est résolu, passer `ready` à `true`.

```jsx
// ...

import {FontAwesomeIcon} from "@fortawesome/react-fontawesome";
import { faSpinner } from "@fortawesome/free-solid-svg-icons"
import { library } from "@fortawesome/fontawesome-svg-core"

library.add(faSpinner);

class FlashCard extends Component {
    constructor() {
        super();
        this.apiHostRoot = "https://aws-services.robertbunch.dev/services";
        this.state = {
            flipClass: "",
            questionData: "",
            ready: false,  // ici
        };
    }

    // ...

    newCard = () => {
        // ...

        axios.get(path).then((response) => {
            this.setState({ questionData: response.data, ready: true }); // ici
        });
    };

	// ...

    render() {
        if(!this.state.ready) { // ici
            this.newCard();
            return (
                <div className="spinner-wrapper">
                    <FontAwesomeIcon icon="spinner" size="6x" spin/> 
                </div>
            );
        }
        return (
            <div className="flash-card">
                // ...
            </div>
        );
    }
}
```

