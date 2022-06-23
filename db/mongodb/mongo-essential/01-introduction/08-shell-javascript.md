# 08 le `mongo shell` et javascript

On peut utiliser le javascript à l'intérieur du `shell`.

Par exemple pour voire les propriété de l'objet `db` :

```js
> Object.entries(db)
[
	[
		"_mongo",
		connection to 127.0.0.1:27017
	],
	[
		"_name",
		"test"
	],
	[
		"_session",
		{
			"id" : UUID("f65550b2-cf45-4854-82b7-89a8f4f5eada")
		}
	]
]
```

