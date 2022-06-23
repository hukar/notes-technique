# 04 Rendu conditionnel

## Utilisation de `&&`

`&&` permet le rendu conditionnel dans les templates :

#### ! c'est une syntaxe `jsx`

```jsx
function Name({ style, children, age }) {
    return (
        <>
            {children && <h3 style={style}>name : {children}</h3>}
            {!children && <p>invalide name</p>}

            {age && <h4>age : {age}</h4>}
            {!age && <p>invalide age</p>}
        </>
    );
}
```

## Alternative `if`

```jsx
function Name({ style, children, age }) {
    if (age >= 40) {
        return (
            <>
                <h3 style={style}>Hello quarantenaire</h3>
            </>
        );
    }
    return (
        <>
            {children && <h3 style={style}>name : {children}</h3>}
            {!children && <p>invalide name</p>}

            {age && <h4>age : {age}</h4>}
            {!age && <p>invalide age</p>}
        </>
    );
}
```

Utile lorsque sous certaine condition tout le template change.

## Les ternaires ` ? : `

```jsx
{girl ? (
  <>
    <br/>
    <span style={{ fontSize: "14px", color: "pink" }}>
      Hello Girl
    </span>
  </>
) : (
  <>
    <br/>
    <span style={{ fontSize: "14px", color: "blue" }}>
      Hello Boy
    </span>
  </>
)}
```

Cela fonctionne bien aussi, et on a pas besoin de plusieurs `return` que je ne trouve pas élégants.,