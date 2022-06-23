https://dev.to/andyrewlee/cheat-sheet-for-updating-objects-and-arrays-in-react-state-48np

# 01 annex Cheat Sheet for Updating Objects and Arrays in React State

### Andrew Lee 

If we want to use arrays or objects in our React state, we have to create a copy of the value before modifying it. This is a cheat sheet on how to do add, remove, and update items in an array or object within the context of managing React state.

## Arrays

```js
const [todos, setTodos] = useState([]);
```

### Add to array

```js
const handleAdd = (todo) => {
  const newTodos = todos.slice();
  newTodos.push(todo);
  setTodos(newTodos);
}
```

The spread operator is syntactic sugar for creating a new copy of a reference.

```js
const handleAdd = (todo) => {
  const newTodos = [...todos];
  newTodos.push(todo);
  setTodos(newTodos);
}
```

We can also use the spread operator to create copy and append an item with the following syntax:

```js
const handleAdd = (todo) => {
  setTodos([...todos, todo]);
}
```

### Remove from array

```js
const handleRemove = (todo) => {
  const newTodos = todos.filter((t) => t !== todo);
  setTodos(newTodos);
}
```

### Update array

```js
const handleUpdate = (index, todo) => {
  const newTodos = [...todos];
  newTodos[index] = todo;
  setTodos(newTodos);
}
```

## Objects

```js
const [todos, setTodos] = useState({});
```

### Add to object

```js
const handleAdd = (todo) => {
  const newTodos = Object.assign({}, todos);
  newTodos[todo.id] = todo;
  setTodos(newTodos);
}
```

We can use spread operator to create shallow copy as well.

```js
const handleAdd = (todo) => {
  const newTodos = {...todos};
  newTodos[todo.id] = todo;
  setTodos(newTodos);
}
```

Similar to arrays, there's a shortcut for doing this in one line:

```js
const handleAdd = (todo) => {
  setTodos({...todos, [todo.id]: todo});
}
```

### Remove from object

```js
const handleRemove = (todo) => {
  const newTodos = {...todos}
  delete newTodos[todo.id]
  setTodos(newTodos);
}
```

### Update object

Same as adding, it will overwrite the value if the key already exists.

```js
const handleUpdate = (todo) => {
  setTodos({...todos, [todo.id]: todo});
}
```

