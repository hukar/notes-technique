## Simuler un href sur un bouton

```html
<button onclick="window.location.href='http://localhost:8080/web-student-tracker/add-student-form.jsp'" class="btn btn-outline-warning">
			Add A Student
		</button>
```

```javascript
window.location.href = 'une/adresse/ou/aller'
```

## return false sur un événement

Renvoyer false sur un événement permet d'annuler le compotement par default.



```html
annuler href
<br />
<a href="http://www.google.com" onclick="return false">google</a>
```

ici empêche le **lien** de fonctionner



```html
<p>
    <form action="#" method="get">
        <input type="text" name="test"/>
        <input type="submit" value="send"  onclick="return true" />
    </form>
</p>
```

ici le **formulaire** n'est pas soumit

cela équivaut a utiliser ```event.preventDefault()```

## Convertir un string en number

```js
const idString = '1';
const idNumber = +idString; // = 1
```

On utilise le `+` comme `parseInt(numString, 10)`

