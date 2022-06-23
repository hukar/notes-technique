# 05 Composant d'ordre supérieur

Une fonction d'ordre supérieur peut prendre en argument une fonction et retourner une fonction.

```
function(function f) {
	return function() { ... }
} 
```

## High Order Component

On peut "Surcharger" un composant en lui attribuant un nouveau fonctionnement :

### Ajouter le texte en vert :

```jsx
const makeGreen = BaseComponent => props => {
    const addGreen = { style: { color: "green" } };
    const newProps = { ...props, ...addGreen };
    return <BaseComponent {...newProps} />;
};

const GreenNameTag = makeGreen(NameTag);
```

```jsx
<NameTag firstName="Pierrot" lastName="Silly" />
<GreenNameTag firstName="Poulet" lastName="Hector" />
```

Le texte sera donc vert.

### Même chose avec une fonction anonyme

```jsx
const makeGreen = function(BaseComponent) { 
  return function(props) {
    const addGreen = { style: { color: "green" } };
    const newProps = { ...props, ...addGreen };
    return <BaseComponent {...newProps} />;
	};
};
```

C'est une fonction qui prend en argument un composant et qui retourne un composant (qui est aussi une fonction renvoyant du `jsx`)

### Empécher l'ajout de style via l'attribut `style`:

```jsx
const removeStyle = BaseComponent => props => {
    const newProps = { ...props, style: {} };
  // const newProps = {...props};
  // delete newProps.style;
    return <BaseComponent {...newProps} />;
};

const NoStyleName = removeStyle(NameTag);
```

```jsx
<NoStyleName firstName="Gloups" lastName="Salamander" style={{ color: "yellow" }} />
```

Le style ne sera pas utilisé.