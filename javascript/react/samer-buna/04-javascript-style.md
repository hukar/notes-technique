# 04 `style` javascript

## `style` property : javascript css

La propriété `style` dans **React** est une propriété spécial qui prend un objet avec les css javascript correspondant :

```jsx
class Card extends React.Component {
  render() {
    return (
      <div
        className="github-profile"
        style={{
          width: "440px",
          padding: "22px",
          margin: "8px",
          border: "grey 1px solid",
          display: "flex",
        }}
      >
        <img src="http://placehold.it/75" style={{ marginRight: "12px" }} />
        <div className="info">
          <div
            className="name"
            style={{ fontSize: "22px", fontWeight: "bold" }}
          >
            name here ...{" "}
          </div>
          <div className="company">Company here ...</div>
        </div>
      </div>
    );
  }
}
```

`camel case` pour les propriétés.

### On peut utiliser les conditions sur les styles

```jsx
class ConditionalStyle extends React.Component {
  render() {
    return (
      <div style={{ color: Math.random() < 0.5 ? "red" : "green" }}>
        I randomly change my color 🧐
      </div>
    );
  }
}
```

<img src="assets/Screenshot2020-08-14at17.40.54.png" alt="Screenshot 2020-08-14 at 17.40.54" style="zoom: 50%;" />

<img src="assets/Screenshot2020-08-14at17.41.01.png" alt="Screenshot 2020-08-14 at 17.41.01" style="zoom:50%;" />
