# 01 Nullable Reference Type

## Afficher les `warning`

Pour pouvoir avoir les `warnings`, on doit ajouter une directive en haut du fichier :

```cs
# nullable enable
```

### Sans

<img src="assets/Screenshot2020-08-11at10.13.42.png" alt="Screenshot 2020-08-11 at 10.13.42" style="zoom: 33%;" />

### Avec

<img src="assets/Screenshot2020-08-11at10.14.01.png" alt="Screenshot 2020-08-11 at 10.14.01" style="zoom: 33%;" />

## demo

`Model.cs`

```cs
# nullable enable

using System;
using System.Collections.Generic;

namespace NullableReferenceType
{
    public class BlogPost
    {
        public string Title { get; set; }
        public List<Comment> Comments { get; set; }
    }

    public class Comment
    {
        public string Body { get; set; }
        public Author PostedBy { get; set; }
    }

    public class Author
    {
        public string Name { get; set; }
        public string Email { get; set; }
    }
}

```

À ce stade un `build` provoque 6 `warnings` :

<img src="assets/Screenshot2020-08-11at10.30.22.png" alt="Screenshot 2020-08-11 at 10.30.22" style="zoom:50%;" />

On nous dit que par défaut ces propriétés sont `Non-nullable`.

L'jout d'un `?` après le type rend celui-ci `nullable`.

```cs
public class BlogPost
{
    public string? Title { get; set; }
    public List<Comment>? Comments { get; set; }
}

public class Comment
{
    public string? Body { get; set; }
    public Author? PostedBy { get; set; }
}

public class Author
{
    public string? Name { get; set; }
    public string? Email { get; set; }
}
```

Ici le `build` ne provoque aucun `warning`.

## Dans la classe `Program.cs`

<img src="assets/Screenshot2020-08-11at10.43.16.png" alt="Screenshot 2020-08-11 at 10.43.16" style="zoom:50%;" />

On a maintenant des `warning` dans la classe `Program.cs`.

Si mes propriétés était `Non-nullable`, je n'aurai pas de `warning` à ce niveau.

#### attention : cela ne change rien pour le `runtime`, si un objet est `null` une exception sera lancée

`!` dit au compilateur qu'on est sûr que notre objet ne sera pas `null` :

<img src="assets/Screenshot2020-08-11at10.49.47.png" alt="Screenshot 2020-08-11 at 10.49.47" style="zoom:50%;" />

Les `warning` ont disparu, mais attention de nouveau ça n'évite pas qu'une exception soit levée au `runtime`.

```cs
# nullable enable
using System;

namespace NullableReferenceType
{


    class Program
    {
        static void Main(string[] args)
        {
            BlogPost post = null;

            PrintPostInfo(post);
        }

        static void PrintTitle(BlogPost post)
        {
            Console.WriteLine(post.Title);
        }

        static void PrintPostInfo(BlogPost post)
        {
            Console.WriteLine($"{post.Title} ({post.Title!.Length})");

            foreach (var comment in post.Comments!)
            {
                var commentPreview = comment.Body!.Length > 10 ?
                    $"{comment.Body.Substring(0, 10)}..." :
                    comment.Body;
                Console.WriteLine(commentPreview);
                Console.WriteLine($"{comment.PostedBy!.Name} ({comment.PostedBy.Email})");
            }
        }
    }
}
```

<img src="assets/Screenshot2020-08-11at10.51.36.png" alt="Screenshot 2020-08-11 at 10.51.36" style="zoom:50%;" />

#### `NullReferenceException`

## Amélioration du code

On ajoute des constructeurs et on initialise la `List<Comment`.

```cs
# nullable enable

using System;
using System.Collections.Generic;

namespace NullableReferenceType
{
    public class BlogPost
    {
        public BlogPost(string title)
        {
            Title = title;
        }

        public string Title { get; set; }
        public List<Comment>? Comments { get; } = new List<Comment>();
    }

    public class Comment
    {
        public Comment(string body, Author postedBy)
        {
            Body = body;
            PostedBy = postedBy;
        }

        public string Body { get; set; }
        public Author PostedBy { get; set; }
    }

    public class Author
    {
        public Author(string name, string email)
        {
            Name = name;
            Email = email;
        }

        public string Name { get; set; }
        public string Email { get; set; }
    }
}
```

Ainsi des warning nous préviennent de possible problème dans `Program.cs` :

 <img src="assets/Screenshot2020-08-11at11.12.20-7137191.png" alt="Screenshot 2020-08-11 at 11.12.20" style="zoom:50%;" />

## Ne pas utiliser les directives

On peut modifier notre fichier `*.csproj` pour afficher les `warning` pour tout le projet sans utiliser les directive `#nullable enable` dans chaque fichier.

```cs
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>netcoreapp3.1</TargetFramework>
    <nullable>enable</nullable>
  </PropertyGroup>

  <PropertyGroup Condition=" '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' ">
    <LangVersion>latest</LangVersion>
  </PropertyGroup>
  <PropertyGroup Condition=" '$(Configuration)|$(Platform)' == 'Release|AnyCPU' ">
    <LangVersion>latest</LangVersion>
  </PropertyGroup>
</Project>
```

#### `<nullable>enable</nullable>`

<img src="assets/Screenshot2020-08-11at11.24.28.png" alt="Screenshot 2020-08-11 at 11.24.28" style="zoom:50%;" />

## Propriété `nullable`

On peut spécifier qu'une propriété peut être `null` au compilateur avec `?` :

Par exemple si on laisse la possibilité à `Email` d'être `null` :

```cs
public class Author
{
    public Author(string name, string? email)
    {
        Name = name;
        Email = email;
    }

    public string Name { get; set; }
    public string? Email { get; set; }
}
```

On aura dans `Program.cs` :

<img src="assets/Screenshot2020-08-11at11.36.42.png" alt="Screenshot 2020-08-11 at 11.36.42" style="zoom:33%;" />

Il suffit d'utiliser le `?` pour enlever le `warning` :

Et il n'y a pas d'erreur au `runtime` :

```cs
using System;

namespace NullableReferenceType
{
    class Program
    {
        static void Main(string[] args)
        {
            var author = new Author("Michel", null);

            var comment = new Comment("hello comment", author);

            var post = new BlogPost("hello nullable");

            post.Comments.Add(comment);

            PrintPostInfo(post);
        }

        static void PrintTitle(BlogPost post)
        {
            Console.WriteLine(post.Title);
        }

        static void PrintPostInfo(BlogPost post)
        {
            Console.WriteLine($"{post.Title} ({post.Title.Length})");

            foreach (var comment in post.Comments!)
            {
                var commentPreview = comment.Body!.Length > 10 ?
                    $"{comment.Body.Substring(0, 10)}..." :
                    comment.Body;
                Console.WriteLine(commentPreview);
                Console.WriteLine($"{comment.PostedBy!.Name} ({comment.PostedBy.Email?.Length})");
            }
        }
    }
}
```

```bash
hello nullable (14)
hello comm...
Michel ()
```

Avec `!` on aurai eu une exception :

<img src="assets/Screenshot2020-08-11at11.43.12.png" alt="Screenshot 2020-08-11 at 11.43.12" style="zoom:50%;" />
