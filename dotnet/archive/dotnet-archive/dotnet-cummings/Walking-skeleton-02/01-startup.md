# 01 Créer le projet `React`

```bash
npx create-react-app client-app --use-npm --template typescript
```

<img src="assets/react-tsx-works.png" alt="react-tsx-works" style="zoom:50%;" />

Notre configuration utilise `NPM` et `Typescript`.

## `tsconfig.json`

C'est le fichier de configuration de `Typescript`.

On laisse `"strict": true` .



## `index.tsx`

Ici aussi il y a un `strict mode` :

```tsx
ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
);
```

On le laisse aussi.

le `Strict Mode` empêche d'utiliser des syntaxes de `React` dépréciées, cela renforce le style de `React 17`.

Il sera peut être nécessaire de le supprimer s'il y a des problèmes avec des package extérieur pas encore mis à jour pour `React 17`.



## `JSX`

Le `jsx` est du sucre syntaxique sur `javascript`, il est transformé en `javascript` avant d'être intégré au `DOM`.

### Différence de syntaxe avec `html`

```html
<p class="ma-super-class"> titi </p>
```

```jsx
<p className="ma-super-class"> titi </p>
```

Car le mot `class` et réservé en `javascript` et que `jsx` est du `javascript`.

```html
<p style="color: red"> titi </p>
```

```jsx
<p style={{color: red}}> titi </p>
```

C'est un objet dans des accolades (pas des doubles accolades).



## React Hook

Les deux `hook` les plus utilisés seront `useState` et `useEffect`.



## Typescript



