# 04 `INSERT `

## Exemple simple

```sql
INSERT INTO Category (Name) VALUES ('Sport'), ('Cooking'), ('Book');
```



### `INTO` est optionnel

```sql
INSERT Category (Name) VALUES ('Sport'), ('Cooking'), ('Book');
```

Cela fonctionne mieux sémantiquement si la table est au singulier.

Certain dialecte `SQL` ne supporte pas cette syntaxe (`Oracle`).



## Exemple d'un `seed database` 

```sql
/***** SEED DATA FOR STATES TABLE *****/
INSERT [dbo].[States] ([Id], [StateName]) VALUES (1, N'Alabama')
INSERT [dbo].[States] ([Id], [StateName]) VALUES (2, N'Alaska')
INSERT [dbo].[States] ([Id], [StateName]) VALUES (4, N'Arizona')
INSERT [dbo].[States] ([Id], [StateName]) VALUES (5, N'Arkansas')
INSERT [dbo].[States] ([Id], [StateName]) VALUES (6, N'California')
INSERT [dbo].[States] ([Id], [StateName]) VALUES (8, N'Colorado')
INSERT [dbo].[States] ([Id], [StateName]) VALUES (9, N'Connecticut')
/* ...
```

On peut ainsi avoir des fichiers de `seed` en `.sql` facilement.
