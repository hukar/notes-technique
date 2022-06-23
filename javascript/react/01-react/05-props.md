# Props

`Component/Greet.js`

```jsx
function Greet(props) {
    console.log(props);
    
    return (
        <>
        <h2>hello greeter {props.name} {props.age || 'old'} {props.color || 'transparent'} {props.power || '/(*_*)\\'}</h2>
        {props.children}
        </>
    )
    
}
```

`<> … </>` permet d'encadrer plusieurs éléments `jsx` sans surcharger le `DOM` avec une `div`

`props.children` sont les élément à l'intérieur des balise de `Greet`

`App.js`

```jsx
<MyGreet name="bruce" age="45">
        <p>I'm the first child</p> <!-- props.children[0] -->
        <p>and me the second child</p> <!-- props.children[1] -->
</MyGreet>
<MyGreet name="michel" color="red"></MyGreet>
<MyGreet name="sania" power="4500"></MyGreet>
```

## Pour une `class`

`Component/Welcome.js`

```jsx
class Welcome extends Component {
    render() {
        return (
          <>
          <h2>
            Welcome {this.props.name} AKA (Also Know As) {this.props.surname}
          </h2>
          {this.props.children}
          </>
        )
    }
}
```

