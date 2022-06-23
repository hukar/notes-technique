# 05 `Form`

`v-form` remplace `form`.

## Les champs

Le `label` est inclus dans le composant `field` :

<img src="assets/Screenshot2020-11-24at11.46.34.png" alt="Screenshot 2020-11-24 at 11.46.34" style="zoom:33%;" />

## `signup` formulaire

### champ : `input`

`<v-text-field *label*="email" *type*="email">` champ de plusieurs types possible

<img src="assets/Screenshot2020-11-24at16.24.42.png" alt="Screenshot 2020-11-24 at 16.24.42" style="zoom:50%;" />

### `autocomplete` : `select`

```html
<v-autocomplete
  label="wich browser do you use ?"
  :items="browsers"
></v-autocomplete>
```

<img src="assets/Screenshot2020-11-24at16.25.33.png" alt="Screenshot 2020-11-24 at 16.25.33" style="zoom:50%;" />

### `File input`

```html
<v-file-input show-size truncate-length="15" v-model="file"></v-file-input>
```

<img src="assets/Screenshot2020-11-24at16.27.19.png" alt="Screenshot 2020-11-24 at 16.27.19" style="zoom:50%;" />

### `date picker`

```html
<v-text-field label="birthday" v-model="birthday" readonly></v-text-field>
<v-date-picker v-model="birthday"></v-date-picker>
```

`readonly` attribut rendant le champ in-inscriptible.

## `checkbox` et `switch`

```html
<v-checkbox label="agree to terms and conditions"></v-checkbox>
<v-switch label="agree to terms and conditions"></v-switch>
```

<img src="assets/Screenshot2020-11-24at16.30.46.png" alt="Screenshot 2020-11-24 at 16.30.46" style="zoom:33%;" />

### Bouton

```html
<v-btn type="submit" color="primary">Submit</v-btn>
```
