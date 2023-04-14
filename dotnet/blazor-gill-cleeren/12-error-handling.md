# 12 La gestion des erreurs



## `ErrorBundary`

<img src="assets/basic-error-no-handling.png" alt="basic-error-no-handling" />

De base en cas d'erreur (d'`exception` lancée), on voit apparaître une barre jaune en bas et un message d'erreur dans la console.

Avec le `tag` `<ErrorBoudary>` on va changer cela.

Je le place directement dans `MainLayout.razor` pour qu'il englobe tous les composants.

```cs
<ErrorBoundary>
	@Body
</ErrorBoundary>
```

On a maintenant une boxe rouge:

<img src="assets/just-error-bundary-added.png" alt="just-error-bundary-added" />



## `ChildContent` et `ErrorContent`

On peut gérer soit même le `template` d'erreur affiché grâce à l'ajout de ces deux `component`:

```html
<ErrorBoundary>
    <ChildContent>
        @Body
    </ChildContent>
    <ErrorContent>
        <MudAlert
            Class="mt-12"
            Variant="Variant.Text"
            Severity="Severity.Error">
            Une erreur c'est produite 👻
        </MudAlert>
    </ErrorContent>
</ErrorBoundary>
```

<img src="assets/producted-error-custom-template.png" alt="producted-error-custom-template" />























