# 04 Vuex

## Rappel

### `mutations : MY_MUTATION(state, payload)` 

Seul endroit où on peut modifier le `state`.

On ne peut pas utiliser du code asynchrone, uniquement du code qui modifie immédiatement le `state`.

On utilise `commit` pour déclencher une mutation.

#### `commit('MY_MUTATION', payload)`



### `actions : myAction({ commit, state, getters, dispatch}, payload)`

C'est ici qu'on lieu les appelle à une API.

Les `actions` ne peuvent pas modifier directement le `state`, il doivent faire appelle à `commit`.

Les `actions` prennent en paramètre le `context` et optionnellement le `paylod`.

le `context` peut être décomposé comme suit : `{ commit, state, getters }`

On `dipatch` une action.

On peut appeler une `action` dans une `action` car on dispose de `dispatch`.

#### `dispatch('myAction', payload)`



### `getters : myGetter(state, getters)`

Permettent de filtrer ou modifier des données présentes dans le `state`.

Un `getters` prend en paramètre le `state` et un lein vers les `getters`.



## Création du `store`

```js
import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
      tasks: [],
  },
  mutations: {
    ADD(state, newTask) {
            state.tasks = [...state.tasks, newTask]
    }, 
    DELETE(state, taskId) {
        state.tasks = state.tasks.filter(task => task.id != taskId)
    },
    MODIFY(state, newTask) {
        const indexTask = state.tasks.findIndex(task => task.id == newTask.id)
        if(indexTask != -1) {
            state.tasks[indexTask] = newTask
            state.tasks = [...state.tasks]
        }
    }
  },
  actions: {
      
      doneTask({ state, commit }, taskId) {
          const taskDone = state.tasks.find(task => task.id == taskId)
          const newTask = { ...taskDone, done: !taskDone.done}
          commit('MODIFY', newTask)
      },
      addTask({ commit, state }, newTaskTitle) {
          if(newTaskTitle) {
            const newTask = { id: Date.now(), title: newTaskTitle, done: false}
            commit('ADD', newTask)
          }
      },
      removeTask({ commit }, taskId) {
          commit('DELETE', taskId)
      }
  }
})
```



## Modification de `Todo.vue`

On peut directement appeler le `$store` dans le template

```html
<v-list
      subheader  
      flat
      v-if="$store.state.tasks.length"
    >
        <v-list-item-group>

            <template v-for="task in $store.state.tasks">
                <v-list-item 
```

### `$store.state.tasks`

On aurait pu aussi le passer en `computed` :

```js
computed: {
    tasks() {
        return this.$store.state.tasks
    }
}
```

### Passage des mutation dans le `template`

On peut directement `commit` depuis le template :

```html
<v-text-field @click:append="$store.commit('ADD', newTitleTask)"
```

Ou utiliser une méthode :

```js
methods: {
      doneTask(taskId) {
          this.$store.dispatch('doneTask', taskId)
      },
      deleteTask(taskId) {
          this.$store.dispatch('deleteTask', taskId)
      },
      addTask() {
        this.$store.dispatch('addTask', this.newTaskTitle)          
      }
```

J'utilise l'organisation `method` => `dispatch: action` => `commit: mutation`

## `Form` et `Vuex`

On peut directement utiliser `commit` sans forcement passer par une action.



## `strict mode`

Il suffit de passer la propriété `strict` à `true` :

```js
const store = new Vuex.Store({
  // ...
  strict: true
})
```

> En mode strict, lorsque l'état de `Vuex` est modifié en dehors des gestionnaires de `mutation`, une erreur sera lancée. Cela permet de s'assurer que toutes les `mutations` du `state` peuvent être explicitement tracées par les outils de débogage.
>
> [lien vers la doc](https://vuex.vuejs.org/fr/guide/strict.html#developpement-vs-production)

Ne pas mettre en production avec le mode `strict` : coût sur les preformances

```js
const store = new Vuex.Store({
  // ...
  strict: process.env.NODE_ENV !== 'production'
})
```

