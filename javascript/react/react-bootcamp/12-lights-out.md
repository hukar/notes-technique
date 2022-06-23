# 12 lights-out

## Correction

**Rappel** attention à `fill` qui rempli de maière statique (ne ré-instancie pas un nouvel objet ou tableau à chaque fois) => utiser `map` pour les tableaux à deux dimensions :

```jsx
this.state = {
      grid: [...Array(this.props.rowSize)].map(() =>
        Array(this.props.colSize).fill(false)
      )
    };
```

Pour copier un tableau à deux dimensions il faut utiliser `JSON` :

```jsx
return { grid: JSON.parse(JSON.stringify(prevState.grid)) }
```

## Générer un tableau à deux dimensions avec des `true` et `false` aléatoires

```jsx
createBoard() {
    let board = [...Array(this.props.nrows)].map(() =>
      [...Array(this.props.ncols)].map(
        () => this.props.chanceLightStartsOn > Math.random() // true ou false
      )
    );

    return board;
  }
```

## Récupérer les coordonnées

Les coordonnées sont de la forme `"2-3"`, ils sont gardées dans la valeur de l'`ID` 

```jsx
let [y, x] = coord.split("-").map(x => Number.parseInt(x));
```

## Intervertir une case

Utilisation d'une fonction pour simplifier l'écriture :

```jsx
function flipCell(y, x) {

  if (x >= 0 && x < ncols && y >= 0 && y < nrows) {
    board[y][x] = !board[y][x];
  }
}

flipCell(x, y);

flipCell(x - 1, y);  // LEFT
flipCell(x + 1, y);  // RIGHT
flipCell(x, y - 1);  // DOWN
flipCell(x, y + 1);  // UP
```

## hasWon

mon `hasWon` :

```jsx
this.setState({
      board: [...board],
      hasWon: !(new Set(board.flat()).has(true))
    });
```

`Board.flat()` pour mettre le tableau sur une dimension

`new Set()` peut prendre un tableau en argument. il comprendra soit {true}, {true, false} ou bien {false}.

`has(true)` si le `Set` possède un seul `true` , la partie n'est pas gagnée.

#### Correction du `hasWon`

```jsx
this.setState({
      board: [...board],
      hasWon: board.every(row => row.every(col => !col))
    });
```

Je ne comprends pas bien la syntaxe !?

