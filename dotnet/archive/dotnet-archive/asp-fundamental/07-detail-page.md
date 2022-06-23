# Création d'une page `Detail`

```bash
dotnet aspnet-codegenerator razorpage Detail Empty -udl -outDir Pages/Restaurants/
```

## le modèle `Detail.cshtml.cs`

```csharp
using Microsoft.AspNetCore.Mvc.RazorPages;
using OdeToFood.Core;

namespace OdeToFood.Pages.Restaurants
{
    public class DetailModel : PageModel
    {
        public Restaurant Restaurant { get; set; }

        public void OnGet()
        {
            Restaurant = new Restaurant { Id = 999, Name = "Toto la crevette", Location = "Liège", Cuisine = CuisineType.Indian };
        }
    }
}
```



## Le template `Detail.cshtml`

```cs
@page
@model OdeToFood.Pages.Restaurants.DetailModel
@{
    ViewData["Title"] = "Detail";
}

<h1>@Model.Restaurant.Name</h1>
<div>@Model.Restaurant.Id</div>
<div>@Model.Restaurant.Location</div>
<div>@Model.Restaurant.Cuisine</div>

<a asp-page="./List" class="btn btn-primary">List of restaurants</a>
```

