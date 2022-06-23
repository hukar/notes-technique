#  08 `DTO`

C'est une couche permettant de découplé nos entités des informations échangées avec le client.

Cela nous donne de la fléxibilité pour validé et assainir (`sanitize`) nos données avant de les transférer dans notre base de données.



## `AutoMapper`

C'est une librairie pour facilité la correspondance antre nos `Dtos` et les classes entités.



## `DTO` Data Transfert Object

On va Créer un dossier `Models`, nos classes `DTO` sont la partie `Model` du pattern `MVC`.

Je préfère nommer ce dossier `Dtos`.

Cette classe va nous permettre d'ajouter des `attribut` de validation.

### Création du fichier `CountryDto.cs` :

```cs
using System.ComponentModel.DataAnnotations;

namespace Dtos
{
    public class CreateCountryDto
    {
        [required]
        [StringLength(maximumlength: 50, ErrorMessage = "Country Name Is Too Long")]
        public string Name { get; set; }
        [required]
        [StringLength(maximumlength: 2, ErrorMessage = "Short Country Name Is Too Long")]
        public string ShortName { get; set; }
    }
    
        public class CountryDto : CreateCountryDto
    {
        public int Id { get; set; }
    }
}
```

L'utilisateur n'intéragit jamais avec la classe `Country`, mais avec `CountryDto`.

On peut avoir un `DTO` par action du contrôleur : `GetCountryDto`, `UpdateCountryDto`, `CreateCountryDto`, ...

Pour une plus grande application chaque `DTO` doit avoir son propre fichier.

### création du fichier `HotelDto.cs`

```cs
using System.ComponentModel.DataAnnotations;

namespace Dtos
{ 
    public class CreateHotelDto
    {
        [Required]
        [StringLength(maximumLength: 150, ErrorMessage = "Hotel Name Is Too Long")]
        public string Name { get; set; }
        [Required]
        public string Address { get; set; }
        [Required]
        [Range(1,5)]
        public double Rating { get; set; }
        [Required]
        public int CountryId { get; set; }
    }
    
    public class HotelDto : CreateHotelDto
    {
        public int Id { get; set; }
        public CountryDto Country { get; set; }
    }
}
```

Les `Dtos` n'ont pas de relation directe avec les objets du domaine (les entités), c'est pourquoi c'est un attribut `CountryDto` et non `Country` dans `HotelDto`.

Le seul lien entre les classes `Dto` et les classes de `Data` sera l'`AutoMapper`.



## On ajoute la liste des hôtel à un `Country`

```cs
// Data/Coutry.cs

public class Country
    {
        // ...
        public virtual IList<Hotel> Hotels { get; set; }
    }
```

Le mot clé `virtual` signifie que l'attrinut peut être substitué dans une classe qui en hérite.

### De même dans `CountryDto`

```cs
public class CountryDto : CreateCountryDto
{
    public int Id { get; set; }
    public IList<HotelDto> Hotels { get; set; }
}
```



# AutoMapper

## Installation

```bash
dotnet add package AutoMapper.Extensions.Microsoft.DependencyInjection --version 8.1.1
```

C'est celui pour `.net 5`.



## Configurations

On va dréer un dossier `Configurations` et une classe `MapperInitializer` pour configurer `AutoMapper`.

```cs
using AutoMapper;
using Data;
using Dtos;

namespace Configurations
{
    public class MapperInitializer : Profile
    {
        public MapperInitializer()
        {
            CreateMap<Country, CoutryDto>().ReverseMap();
            CreateMap<Country, CreateCoutryDto>().ReverseMap();
			CreateMap<Hotel, HotelDto>().ReverseMap();
			CreateMap<Hotel, CreateHotelDto>().ReverseMap();
        }
    }
}
```

On va maintenant enregistrer notre `MapperInitializer` dans `Startup.cs` :

```cs
// ...
services.AddCors(o => o.AddPolicy("AllowAllPolicy", builder => builder
                                  .AllowAnyOrigin()
                                  .AllowAnyMethod()
                                  .AllowAnyMethod()));

services.AddAutoMapper(typeof(MapperInitializer));  // <=

// ...
```

