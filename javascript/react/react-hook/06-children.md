# 06 Children

## Props `children`

L'objet `props` possède l'attribut `children` qui correspond au contenu entre les balises du composant :

```jsx
function Name({ style, children }) {
    return <h3 style={style}>{children}</h3>;
}
```

Dans le template parent :

```jsx
{["jul", "jym", "jane"].map(name => (
  <Name style={styleName} key={name}>
    {name}
  </Name>
))}
```

## `children` comme attribut

On peut aussi définir `children` comme un attribut en `jsx`

```jsx
function BaseElement(props) {
    return (
        <div children={<span color="red">Toto est parti à l'école</span>}></div>
    );
}
```

est la même chose que :

```jsx
function BaseElement(props) {
    return (
        <div>
            <span color="red">Toto est parti à l'école</span>
        </div>
    );
}
```

## Avec l'opérateur de décomposition `spread attribute`

```jsx
function Elt(props) {
    return (
        <>
            <p {...props}></p>
            <p {...props}></p>
        </>
    );
}
```

Et dans le composant parent :

```jsx
<Elt
  style={{ color: "red", fontSize: "26px" }}
  className="superP"
>
  Hello les gars
</Elt>
```

=> le html obtenu :

```html
<p class="superP" style="color: red; font-size: 26px;">Hello les gars</p>
<p class="superP" style="color: red; font-size: 26px;">Hello les gars</p>
```

