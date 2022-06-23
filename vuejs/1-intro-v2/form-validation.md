## Form Validation

```html
<div v-if="errors.length">
    <h3>Errors:</h3>
    <ul>
        <li v-for="error in errors">{{error}}</li>
    </ul>
</div>
```

```js
data() {
    return {
        name: null,
        review: null,
        rating: null,
        errors: [],  // handling form errors
    };
},
```

```js
methods: {
        submitReview() {
            this.errors = [];  // reset errors
            
            // test if the form is valid
            if (this.name && this.review && this.rating) {
                const formReview = {
                    name: this.name,
                    review: this.review,
                    rating: this.rating,
                };
                this.$emit("review-submitted", formReview);
                this.name = null;
                this.review = null;
                this.rating = null;
            } else {
                // adding errors
                if (!this.name) this.errors.push("name is required");
                if (!this.review) this.errors.push("review is required");
                if (!this.rating) this.errors.push("rating is required");
            }
        },
```

