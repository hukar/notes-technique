# `virtual`, `new` et `override`

## 1 Rien

```cs
class Animal
{
    public void Running()
    {
        Console.WriteLine("I'm running\n");
    }
}

class Cat : Animal
{
    public void Running()
    {
        Console.WriteLine("Miamiaouw, I'm running ...\n");
    } 
}
```

On ré-écrit la méthode sans aucun mots clés.

```
warning CS0108: 'Cat.Running()' hides inherited member 'Animal.Running()'. Use the new keyword if hiding was intended.
```

On a un `warning`.

```cs
Animal animal = new Animal();
Cat chat = new Cat();
Animal animalChat = new Cat();

Console.WriteLine("animal :");
animal.Running();

Console.WriteLine("chat :");
chat.Running();

Console.WriteLine("animal chat :");
animalChat.Running();
```

```
animal :
I'm running

chat :
Miamiaouw, I'm running ...

animal chat :
I'm running
```

#### Seul l'objet déclaré de type dérivée profite de la nouvelle implémentation



## 2 `new`

```cs
class Animal
{
    public void Running()
    {
        Console.WriteLine("I'm running\n");
    }
}

class Cat : Animal
{
    public new void Running()
    {
        Console.WriteLine("Miamiaouw, I'm running ...\n");
    } 
}
```

Plus de `warning`

```
animal :
I'm running

chat :
Miamiaouw, I'm running ...

animal chat :
I'm running
```

On obtient le même résultat qu'avec rien, sauf qu'on a plus de `warning`.



## 3 `virtual` et `override`

Pour utiliser `override` dans une classe dérivée, il faut que la méthode de base soit marquée `virtual`, `static` ou `override`.

```cs
class Animal
{
    public virtual void Running()
    {
        Console.WriteLine("I'm running\n");
    }
}

class Cat : Animal
{
    public override void Running()
    {
        Console.WriteLine("Miamiaouw, I'm running ...\n");
    } 
}
```

```cs
Animal animal = new Animal();
Cat chat = new Cat();
Animal animalChat = new Cat();

Console.WriteLine("animal :");
animal.Running();

Console.WriteLine("chat :");
chat.Running();

Console.WriteLine("animal chat :");
animalChat.Running();
```

```
animal :
I'm running

chat :
Miamiaouw, I'm running ...

animal chat :
Miamiaouw, I'm running ...
```

#### Tous les objets instanciés du type dérivé profite de la nouvelle implémentation

## Conclusion 

Pour obtenir un `polymorphisme` c'est le type à l'instanciation qui détermine quelle implémentation de méthode doit être utilisée.

`new` ne permet pas le polymorphisme (ne rien mettre non plus).

`virtual` et `override` sont des mots clés qui permettent de mettre en oeuvre le `polymorphisme`.