# 08 `Render Fragment`

On veut rendre le contenu entre les `balises` d'un `component`:

```cs
<MudText Typo="Typo.h5">My Component</MudText>

@ChildContent

@code {
    [Parameter]
    public RenderFragment? ChildContent { get; set; }
}
```

Il faut utiliser l'`attribute` : `[Parameter]`.

> On doit nommer la propriété `ChilContent`, si je le nomme autrement:
>
> ```cs
> <MudText Typo="Typo.h5">My Component</MudText>
> 
> @SomeFragment
> 
> @code {
>     [Parameter]
>     public RenderFragment? SomeFragment { get; set; }
> }
> ```
>
> J'obtiens une exception:
>
> <img src="assets/child-content-namming-convention-mandatory.png" alt="child-content-namming-convention-mandatory" />

Dans une `Page`:

```html
<MyComponent>Hey Guy, How are you</MyComponent>
```

<img src="assets/little-fragment-rended-on-home-page.png" alt="little-fragment-rended-on-home-page" style="zoom:67%;" />

On peut aussi lui passer un autre `component`:

```html
<MyComponent> <CatDisplay /> </MyComponent>
```

<img src="assets/component-in-render-fragment-cat-displey.png" alt="component-in-render-fragment-cat-displey" style="zoom: 33%;" />