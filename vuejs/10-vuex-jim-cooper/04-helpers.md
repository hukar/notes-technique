# 04 Vuex `map helpers`

Ce sont des raccourcies syntaxiques.

## `mapState`

<img src="assets/map-state.png" alt="map-state" style="zoom:50%;" />

#### ! à l'utilisation du spread operator `...`

### `namespaced modules`

<img src="assets/map-state-namespaced-module.png" alt="map-state-namespaced-module" style="zoom:50%;" />

### `non-namespaced modules`

<img src="assets/non-namespaced-modules.png" alt="non-namespaced-modules" style="zoom:50%;" />

On passe un objet comme option.

On peut choisir le nom de *mappage* comme bon nous semble.

### exemple

```js
import { mapState } from 'vuex'

// ...

computed: {
    ...mapState('users', ['user']),
        //   user() {
        //       return this.$store.state.users.user
        //   }
}
```



## `mapGetters`

<img src="assets/map-getters.png" alt="map-getters" style="zoom:50%;" />



### `non-namespaced`

<img src="assets/map-getters-non-namespaced.png" alt="map-getters-non-namespaced" style="zoom:50%;" />

Il n'y a plus les `arrow function`.

C'est la même syntaxe que pour le `root store`.

### `namespaced`

<img src="assets/Screenshot 2020-12-22 at 16.48.08.png" alt="Screenshot 2020-12-22 at 16.48.08" style="zoom:50%;" />



### exemple

```js
import { mapGetters } from 'vuex'

// ...

computed: {
    ...mapGetters('products', ['getFilterProducts']),
        visibleProducts() {
        return this.getFilterProducts(this.filter)
    },
        // visibleProducts() {
        //     return this.$store.getters['products/getFilterProducts'](this.filter)
        // },
},
```

C'est peut être pas intéressant dans ce cas mais cela montre :

- qu'on peut mélanger les `computed property` avec les `mapGetters`
- On peut utiliser les `mapGetters` comme n'importe quelle `computed property` et les appeler n'importe où dans le composant.



## `mapMutations`

`mapMutations` et `mapActions` fonctionnent comme `mapGetters` à la différence qu'ils se placent dans `methods` au lieu de `computed`.

<img src="assets/map-mutations.png" alt="map-mutations" style="zoom:50%;" />

On peut les utiliser dans d'autres méthodes :

<img src="assets/use-map-mutations.png" alt="use-map-mutations" style="zoom:50%;" />

La syntaxe reste la même pour les `modules` `non-namespaced`.

### `namespaced modules`

<img src="assets/nmespaced-map-mutations.png" alt="nmespaced-map-mutations" style="zoom:50%;" />

### exemple

```js
import { mapState, mapMutations } from 'vuex'

// ...

methods: {
        ...mapMutations('users', ['ADD_USER']),
        signOut() {
        this.showMenu = false;
        //   this.$store.commit('users/ADD_USER', null)
        this.ADD_USER(null)
    },
},
```



## `mapActions`

<img src="assets/non-namespaced-map-actions.png" alt="non-namespaced-map-actions" style="zoom:50%;" />

Si l'action renvoie une `promise`, on la traiter dans la méthode (exemple des erreurs).

<img src="assets/namespaced-map-actions.png" alt="namespaced-map-actions" style="zoom:50%;" />



### Exemple

```js
import { mapActions } from 'vuex'

// ...

methods: {
    ...mapActions('users', ['registerUser']),
    registerUser() {
        this.registerError = false
        this.saving = true;
        const user = { 
            firstName: this.firstName, 
            lastName: this.lastName, 
            email: this.email,
            password: this.password, 
        }
        
        // this.$store.dispatch('users/registerUser', user)
        this.registerUser(user)
            .then(() => this.$router.push({name: 'Products'}))
            .catch(() => this.registerError = true)
```

#### ! il y a conflit de nom

Je vais utiliser la syntaxe permettant de renommer son `action` :

```js
...mapActions({
    register: 'users/registerUser'
}),
registerUser() {
// ...
    this.register(user)
        .then(() => this.$router.push({name: 'Products'}))
        .catch(() => this.registerError = true)
```

