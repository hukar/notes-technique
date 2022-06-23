## Tabs

```jsx
const eventBus = new Vue();

Vue.component("products-tab", {
    props: {
        reviews: {
            type: Array,
            required: true,
        },
    },
    template: /*html*/ `
    <div>
        <span 
            class="tab" 
            :class="{activeTab: selectedTab === tab}"
            v-for="(tab, index) in tabs" 
            :key="index"
            @click="selectedTab = tab">{{tab}}</span>
            <review-list v-show="selectedTab === 'reviews'" v-if="reviews.length" :reviews="reviews"></review-list>
            <product-review v-show="selectedTab === 'make a review'"></product-review>
    </div>
    
    `,
    data() {
        return {
            tabs: ["reviews", "make a review"],
            selectedTab: "reviews",
        };
    },
});
```



Utilisation de `v-show` pour alterner entre les `tabs` sans reconstruire le `DOM`.

## Event bus (communication inter composant)

Un enfant ne peut pas communiquer avec un grand-parent.

Il faut implémenter un bus de transmission `event bus`.

```js
const eventBus = new Vue(); // tout en haut du code accessible à tous les composants
```

Dans le composant formulaire on va émettre de façon générale :

```js
submitReview() {
    this.errors = [];
    if (this.name && this.review && this.rating) {
        const formReview = {
            name: this.name,
            review: this.review,
            rating: this.rating,
        };
        eventBus.$emit("review-submitted", formReview);
        // ...
```

#### `eventBus.$emit("my-event", data)`

Maintenant on va utiliser ce bus dans le composant `Product` pour écouter l'événement :

```js
,
    mounted() {
        eventBus.$on("review-submitted", (review) => {
            this.reviews.push(review);
        });
    },
```

#### `eventBus.$on("my-event", data => { ... })`

