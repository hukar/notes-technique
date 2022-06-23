# AA. Ajouter les fichiers `vue` à l'extension `toggle-quotes`

Le fichier à modifier se trouve : `/Users/kar/.vscode/extensions/britesnow.vscode-toggle-quotes-0.3.3/package.json`.

C'est dans `package.json` => `configurationDefault` que l'on doit ajouter les fichiers `vue`

```json
"configurationDefaults": {
			"[typescript]": {
				"togglequotes.chars": [
					"\"",
					"'",
					"`"
				]
			},
			// ...
			"[markdown]": {
				"togglequotes.chars": [
					"\"",
					"'",
					"`"
				]
			},
			"[vue]": {
				"togglequotes.chars": [
					"\"",
					"'",
					"`"
				]
			}
		},
		"commands": [
```

