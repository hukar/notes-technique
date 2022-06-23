# 05 child component

## Importer un composant

Deux méthodes :

### 1 `import from`

```js
import FieldAddTask from '@/components/todo/FieldAddTask'

export default {
  name: 'Todo',
  components: { FieldAddTask },
```

je préfère cette méthode.

### 2 `require`

```js
export default {
  name: 'Todo',
  components: { 
      FieldAddTask: require('@/components/todo/FieldAddTask').default 
    },
```



## Passer une `props`

### Syntax Array

```js
props: [ 'task' ]
```



### Syntax Object

```js
props: {
    task: {
        type: Object,
            required: true
    }
},
```

