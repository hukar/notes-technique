# 06 `componentWillUnmount`

Cette méthode est appelée lorsque le composant quitte le `DOM`.

Elle sert à *nettoyer* les effets du composant sortant, par exemple un timer.

```js
import React, { Component } from "react";

class Timer extends Component {
    constructor() {
        super();
        this.state = {
            time: 0,
        }
    }

    componentDidMount() {
        this.timerId = setInterval(() => {
            this.setState({ time: this.state.time + 1 });
            console.log(this.state.time)
        }, 1000);  
    }

    componentWillUnmount() {
        clearInterval(this.timerId);
    }

    render() {
        return <h1>{this.state.time}</h1>;
    }
}

export default Timer;
```

