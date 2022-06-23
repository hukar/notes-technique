# 02 Utilisation avec `postcss`

## Installation et mise en place

```bash
npm i tailwindcss
```

Puis création de `src/style.css`

```css
@tailwind base;

@tailwind components;

@tailwind utilities;
```

C'est dans ce fichier que l'on va créer ses classes :

```css
@tailwind base;

@tailwind components;

@tailwind utilities;

.input {
    @apply bg-gray-200 outline-none shadow-inner px-3 py-1 w-full border-b-4 border-blue-200 text-gray-600;
}

.input:focus {
    @apply border-blue-700;
}
.input-label {
    @apply block mb-2 text-gray-500;
}
```

## création du fichier de `config`

```bash
npx tailwindcss init --full
```

`--full` pour avoir toutes les règles configurables.

## générer son `css`

Pour obtenir son fichier `css`

```bash
npx tailwindcss build src/style.css -o dist/style.css
```

et bien sûr dans le `html` :

```html
<!-- <link href="https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css" rel="stylesheet"> -->
    <link rel="stylesheet" href="dist/style.css">
```

Plus haut commenté c'est le `cdn` pour un rapide prototype ou des tests.

## Les classes `responsives`

```css
.card {
    @apply bg-white shadow-lg max-w-lg my-4;
}

@screen md {
    .card {
        @apply flex;
    }
}
```

#### `@screen md { .myClass { @apply ... } }` 

