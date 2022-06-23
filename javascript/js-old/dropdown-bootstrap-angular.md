# Un dropdown avec Bootsrap et Angular

L'idée c'est de n'utiliser que le css de Bootstrap et de ne pas ajouter de dépendance JS à Angular.

`component.html :`

```html
<div class="dropdown">
    <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" (click)="onDropDown($event,dropdownMenu)">
        Dropdown button
    </button>
    <div class="dropdown-menu" #dropdownMenu>
        <a class="dropdown-item" href="#">Action</a>
        <a class="dropdown-item" href="#">Another action</a>
        <a class="dropdown-item" href="#">Something else here</a>
    </div>
</div>
```

On crée une référence locale avec `#dropdownMenu` et on lie l'événement `click` en lui passant l'objet `$event` créé par Angular et notre `dropdown-menu`

`component.ts :`

```typescript
onDropDown(evt: Event, elt: HTMLElement) {
    const dropFun = function() {

        evt.stopPropagation();

        elt.classList.remove('show');
        document.removeEventListener("click", dropFun)
    }

    evt.stopPropagation();

    elt.classList.toggle('show');
    document.addEventListener("click", dropFun)
}
```

On place un écouteur de `click` sur le document qu'on retire dès qu'il est cliqué.

Surtout bien arrêter le *bubbling* avec `evt.stopPropagation()`