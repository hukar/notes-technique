# Pourquoi le state ne doit pas être modifié directement ?

Dans React on dit que le state doit être immuable, en d'autre termes qu'il change par copie et non par référence.

### Pas bien

```jsx
this.state.listItem.push(newItem);
```

### Bien

```jsx
this.setState(prevState => {
  return {listItem: [...prevState.listItem, newItem]}
});
```

## Tester le problème

```jsx
class Mutability extends Component {
  constructor(props) {
    super(props);

    this.state = {
      items: []
    };

    this.addItemMutably = this.addItemMutably.bind(this);
    this.addItemImmutably = this.addItemImmutably.bind(this);
  }

  makeItem() {
    return {
      one: Math.floor(Math.random() * 10) + 1,
      two: Math.floor(Math.random() * 20) + 1
    };
  }

  addItemMutably() {
     this.state.items.push(this.makeItem()); 
  }

  addItemImmutably() {
    this.setState(prevState => {
      return { items: [...prevState.items, this.makeItem()] };
    });
  }

  render() {
    return (
      <div>
        <button onClick={this.addItemMutably}>Add Mutably</button>
        <button onClick={this.addItemImmutably}>Add Immutably</button>
        {this.state.items.map((o, i) => (
          <p key={i}>
            {o.one} : {o.two}
          </p>
        ))}
      </div>
    );
  }
}
```

Ici très clairement `addItemMutably` ne déclenche pas la mise à jour du rendu, même si on ajoute `render` dans la méthode:

```jsx
addItemMutably() {
  this.state.items.push(this.makeItem());
  this.render();
}
```

La vue ne se met pas plus à jour !

### Par contre en utilisant setState mais en mutant le tableau :

```jsx
addItemMutably() {
  this.setState(prevState => {
    prevState.items.push(this.makeItem());
    return { items: prevState.items };
  });
}
```

Ici tout semble aller correctement, alors pourquoi utiliser `items: [...prevState.items]` ??

## Résumé suggéré par React

Ne jamais modifier le state, toujours utiliser `setState`, considérer le state comme immuable.

En interne React compare les objets avec `===` ou `!==`, si on a pas changer l'objet par 