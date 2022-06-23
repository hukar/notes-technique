# 00 Les fonctionnalités `Vuejs` pas connu

## `v-model.trim`

```html
<input v-model.trim="email" placeholder="Email..." />
```

Permet d'enlever les espaces avant et après.



## `this.$router.push(<route>)`

Pour naviguer.

```js
this.$router.push({name: 'Products'})
```



## JSDoc syntaxe

```js
/**
 * 
 * @param {string} filter 
 * @param {product[]} products 
 * @return {product[]}
 */

export default function filterProducts(filter, products) {
  if (!filter) { return products; }

  return products.filter((p) => p.category === filter);
}
```



## Autofill

Dans Chrome :

>https://stackoverflow.com/questions/15738259/disabling-chrome-autofill
>
>EDIT Sept 2020 : `autocomplete="chrome-off"` désactive l'auto-remplissage dans **Chrome**. Réponse originale ci-dessous.
>
>### Réponse originale
>
>Pour les nouvelles versions de **Chrome**, vous pouvez simplement mettre `autocomplete="new-password"` dans votre champ de mot de passe et c'est tout. J'ai vérifié, ça marche bien.
>
>J'ai eu ce conseil d'un développeur de Chrome lors de cette discussion : https://bugs.chromium.org/p/chromium/issues/detail?id=370363#c7
>
>P.S. Notez que Chrome tentera de déduire le comportement de remplissage automatique à partir de `name`, `id` et de tout contenu textuel qu'il peut obtenir autour du champ, y compris les `label` et les nœuds de texte arbitraires. S'il existe un `autocomplete token` comme `street-address` dans le contexte, Chrome le remplira automatiquement. 
>
>L'heuristique peut être assez déroutante car elle ne se déclenche parfois que s'il y a des champs supplémentaires dans le formulaire, **ou pas s'il y a trop peu de champs dans le formulaire**. 
>
>Notez également que `autocomplete="no"` semble fonctionner mais que `autocomplete="off"` ne fonctionne pas pour des raisons historiques.
>
> `autocomplete="no"` indique au navigateur que ce champ doit être rempli automatiquement comme un champ appelé `"no"`. Si vous générez des noms aléatoires uniques pour l'autocomplétion, vous désactivez l'autocomplétion.
>
>Si vos utilisateurs ont visité de mauvais formulaires, leurs informations de remplissage automatique peuvent être corrompues. Il peut s'avérer nécessaire pour eux de corriger manuellement leurs informations de remplissage automatique dans Chrome.
>
>Traduit avec www.DeepL.com/Translator (version gratuite)



### Pour ajouter le remplissage automatique (qui est fort pratique en fait)

```html
<input v-model.trim="email" placeholder="Email..." autocomplete="email" />
```

#### `autocomplete="<name_field>"`

Explications de **Google** :

https://developers.google.com/web/fundamentals/design-and-ux/input/forms#recommended_input_name_and_autocomplete_attribute_values



Article **Stackoverflow** :

> [Voici un lien vers la documentation du WHATWG pour l'activation de l'autocomplétion.](https://html.spec.whatwg.org/multipage/forms.html#attr-fe-autocomplete)
>
> Google a rédigé [un guide assez sympa](https://developers.google.com/web/fundamentals/design-and-ui/input/forms/#recommended_input_name_and_autocomplete_attribute_values) pour le développement d'applications web adaptées aux appareils mobiles. Ils ont une section sur la façon de nommer les entrées sur les formulaires pour utiliser facilement l'auto-remplissage. Même si ce guide est écrit pour les mobiles, il s'applique aussi bien aux ordinateurs de bureau qu'aux mobiles !
>
> # Comment activer l'auto-remplissage dans vos formulaires `HTML`
>
> Voici quelques points clés sur la façon d'activer `autocomplete`:
>
> - **Utiliser les `<label>` pour toutes vos balises `<input>`**
>
> - **Ajouter l'attribut `autocomplete` à vos balises `<input>` et suivez ce  [guide](https://developers.google.com/web/fundamentals/design-and-ui/input/forms/#recommended_input_name_and_autocomplete_attribute_values) pour le remplir correctement.** 
>
> - **Nommer les attributs `name` et `autocomplete` correctement pour toutes les balises `<input>`**
>
> - **Exemple**:
>
> 	```html
> 	<label for="frmNameA">Name</label>
> 	<input type="text" name="name" id="frmNameA"
> 	placeholder="Full name" required autocomplete="name">
> 	
> 	<label for="frmEmailA">Email</label>
> 	<input type="email" name="email" id="frmEmailA"
> 	placeholder="name@example.com" required autocomplete="email">
> 	
> 	<!-- note that "emailC" will not be autocompleted -->
> 	<label for="frmEmailC">Confirm Email</label>
> 	<input type="email" name="emailC" id="frmEmailC"
> 	placeholder="name@example.com" required autocomplete="email">
> 	
> 	<label for="frmPhoneNumA">Phone</label>
> 	<input type="tel" name="phone" id="frmPhoneNumA"
> 	placeholder="+1-555-555-1212" required autocomplete="tel">
> 	```
>
> # Comment nommer vos balises `<input>` 
>
> Afin de déclencher la saisie automatique, assurez-vous que vous nommez correctement les atributs `name` et `autocomplete` dans votre balise `<input>`. Cela permettra de déclencher automatiquement l'autocomplétion sur les formulaires. Assurez-vous également d'avoir une balise `<label>`! Cette information se trouve également [ici](https://developers.google.com/web/fundamentals/design-and-ui/input/forms/#recommended_input_name_and_autocomplete_attribute_values).
>
> #### Voici comment nommer vos `inputs`:
>
> - Name
>
> 	- Use any of these for **`name`**: `name fname mname lname`
>
> 	- Use any of these for `autocomplete` :
> 		- `name` (for full name)
> 		- `given-name` (for first name)
> 		- `additional-name` (for middle name)
> 		- `family-name` (for last name)
>		
> 	- Example: `<input type="text" name="fname" autocomplete="given-name">`
>
> - Email
>
> 	- Use any of these for **`name`**: `email`
> 	- Use any of these for **`autocomplete`**: `email`
> 	- Example: `<input type="text" name="email" autocomplete="email">`
>
> - Address
>
> 	- Use any of these for **`name`**: `address city region province state zip zip2 postal country`
>
> 	- Use any of these for `autocomplete` :
> 		- For one address input:
> 			- `street-address`
> 		- For two address inputs:
> 			- `address-line1`
> 			- `address-line2`
> 		- `address-level1` (state or province)
> 		- `address-level2` (city)
> 		- `postal-code` (zip code)
> 		- `country`
>
> - Phone
>
> 	- Use any of these for **`name`**: `phone mobile country-code area-code exchange suffix ext`
> 	- Use any of these for **`autocomplete`**: `tel`
>
> - Credit Card
>
> 	- Use any of these for **`name`**: `ccname cardnumber cvc ccmonth ccyear exp-date card-type`
>
> 	- Use any of these for `autocomplete` :
> 		- `cc-name`
> 		- `cc-number`
> 		- `cc-csc`
> 		- `cc-exp-month`
> 		- `cc-exp-year`
> 		- `cc-exp`
> 		- `cc-type`
>
> - Usernames
>
> 	- Use any of these for **`name`**: `username`
> 	- Use any of these for **`autocomplete`**: `username`
>
> - Passwords
>
> 	- Use any of these for **`name`**: `password`
>
> 	- Use any of these for  `autocomplete` :
> 		- `current-password` (for sign-in forms)
> 		- `new-password` (for sign-up and password-change forms)
>
> # Resources
>
> - [Current WHATWG HTML Standard for autocomplete.](https://html.spec.whatwg.org/multipage/forms.html#attr-fe-autocomplete)
> - ["Create Amazing Forms" from Google](https://developers.google.com/web/fundamentals/design-and-ui/input/forms/#recommended_input_name_and_autocomplete_attribute_values). Seems to be updated almost daily. Excellent read.
> - ["Help Users Checkout Faster with Autofill" from Google](https://developers.google.com/web/updates/2015/06/checkout-faster-with-autofill) in 2015.