# `@apply`

On peut regrouper des utilitaires en classe composant dans le fichier `tailwind.css`

```css
@tailwind base;
@tailwind components;

.btn {
    @apply inline-block bg-indigo-500  text-white px-5 py-3 rounded-lg shadow-lg uppercase tracking-wider font-semibold text-sm 
}

@tailwind utilities;
```

## Avec les pseudo-classes

```css
.btn:hover {
    @apply bg-indigo-300;
}

.btn:focus {
    @apply shadow-outline;
}

.btn:active {
    @apply bg-indigo-700;
}
```

## avec les break point responsive

```css
@screen sm {
    .btn:hover {
        @apply text-4xl;
    }
}
```

#### effectuer un re-`build` après chaque changement (ou utiliser `--watch`).



## Pourquoi placer ses classes entre `components` et `utilities`

Pour pouvoir *overider* notre classe avec les `utilities` :

```html
<button class="btn px-8">
    Hello
</button>
```

La classe `px-8` est prise en compte car `@tailwind utilities` est placé après notre classe `.btn`.



## Mixer classes utilitaires et classes composant

Certaine classe peuvent être plus pratique gardée en dehors du composant, comme un `media-query` ou une ombre :

`tailwind.css`

```css
@tailwind base;
@tailwind components;

.btn {
    @apply inline-block bg-indigo-500  text-white px-5 py-3 rounded-lg uppercase tracking-wider font-semibold text-sm;
}

.btn:hover {
    @apply bg-indigo-300;
}

.btn:focus {
    @apply shadow-outline;
}

.btn:active {
    @apply bg-indigo-700;
}

@tailwind utilities;
```

`index.html`

```html
<a class="btn sm:hover:text-4xl shadow-lg" href="#">Book your escape</a>
```



## Multi class component pattern

Si on veut plusieurs couleur de bouton on aurait tendance à faire ceci :

```css
.btn-indigo {
    @apply inline-block bg-indigo-500  text-white px-5 py-3 rounded-lg uppercase tracking-wider font-semibold text-sm;
}
.btn-indigo:hover {
    @apply bg-indigo-300;
}
.btn-indigo:focus {
    @apply shadow-outline;
}
.btn-indigo:active {
    @apply bg-indigo-700;
}

.btn-gray {
    @apply inline-block bg-gray-400  text-gray-700 px-5 py-3 rounded-lg uppercase tracking-wider font-semibold text-sm;
}
.btn-gray:hover {
    @apply bg-gray-200;
}
.btn-gray:focus {
    @apply shadow-outline;
}
.btn-gray:active {
    @apply bg-gray-600;
}
```

Le problème avec cette approche est que si on veut changer le `padding` du bouton, le code se trouve à plusieurs endroits.

On va donc extraire une classe abstraite du bouton :

```css
.btn {
    @apply inline-block  px-5 py-3 rounded-lg uppercase tracking-wider font-semibold text-sm
}
.btn:focus {
    @apply shadow-outline;
}

.btn-indigo {
    @apply bg-indigo-500  text-white;
}
.btn-indigo:hover {
    @apply bg-indigo-300;
}
.btn-indigo:active {
    @apply bg-indigo-700;
}

.btn-gray {
    @apply bg-gray-400  text-gray-700;
}
.btn-gray:hover {
    @apply bg-gray-200;
}
.btn-gray:active {
    @apply bg-gray-600;
}
```

Et dans le `html` utiliser deux classes :

```html
<a class="btn btn-indigo sm:hover:text-4xl shadow-lg" href="#">Book your escape</a>

<a class="ml-2 btn btn-gray sm:hover:text-4xl" href="#">More choice ...</a>
```

