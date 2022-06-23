# 04 Validation

## Utilisation de `Fluent Validation`

```bash
dotnet add package FluentValidation.DependencyInjectionExtensions --version 10.3.3
```

On l'installe toujours dans le projet `Application`.

La validation a lieu sur les `DTO` en écriture, la lecture n'a pas besoin de validation.



## Créer un `Validator` : `CreateLeaveTypeDtoValidator`

Dans `DTO/LeaveType` on crée un dossier `Validators` et dedans la classe `CreateLeaveTypeValidator`.

```cs
namespace Application.DTOs.LeaveType.Validators
{
    public class CreateLeaveTypeDtoValidator : AbstractValidator<CreateLeaveTypeDto>
    {
        public CreateLeaveTypeDtoValidator()
        {
            RuleFor(p => p.Name)
              .NotEmpty().WithMeassage("{PropertyName} is required")
              .NotNull()
              .MaximumLength(50).WithMessage("{PropertyName} must not exceed {ComparaisonValue} characters");
          	RuleFor(p => p.DefaultDays)
              .NotEmpty().WithMessage("{PropertyName} is required")
              .GreaterThan(0).WithMessage("{PropertyName} must be at least 1")
              .LessThan(100).WithMessage("{PropertyName} must be less than {ComparaisonValue}");
        }
    }
}
```

`{PropertyName}` renvoie le nom de la propriété.

`{ComparaisonVale}` renvoie la valeur avec laquelle on compare la propriété.

`NotNull()`

`NotEmpty()`

`MaximumLength(nbMax)`

`WithMessage(message)`

`GreaterThan(int)`

`LessThan(int)`

## Appliquer le `Validator`

C'est dans le `Handler` de la `Command` : `CreateLeaveTypeCommandHandler`

```cs
public class CreateLeaveTypeCommandHandler : IRequestHandler<CreateLeaveTypeCommand, int>
{
  // ...

  public async Task<int> Handle(CreateLeaveTypeCommand request, CancellationToken cancellationToken)
  {
    var validator = new CreateLeaveTypeDtoValidator();
    var validationResult = await validator.ValidateAsync(request.LeaveTypeDto);
    
    if(validationResult.IsValid == false)
    {
      throw new Exception();
    }
    
    var leaveType = _mapper.Map<Domain.LeaveType>(request.CreateLeaveTypeDto);
    leaveType = await _leaveTypeRepository.Add(leaveType);

    return leaveType.Id;
  }
}
```



## Création de `CreateLeaveRequestDtoValidator`

```cs
public class CreateLeaveRequestDtoValidator : AbstractValidator<CreateLeaveRequestDto>
{
  public CreateLeaveRequestDtoValidator()
  {
    RuleFor(p => p.StartDate)
      // .GreaterThan(DateTime.Now)
      .LessThan(p => p.EndDate).WithMessage("{property} must be before {ComparaisonValue}");
    
    // Pas obligatoire avec la règle du dessus
    RuleFor(p => p.EndDate)
      .GreaterThan(p => p.StartDate).WithMessage("{PropertyName} must be after {ComparaisonValue}");
    
    RuleFor(p => p.LeaveTypeId)
      .GreaterThan(0)
      .MustAsync(async (id, token) => {
        // On doit appeller la DB
      }).WithMessage("{PropertyName} does not exist");
  }
}
```

On doit injecter un `Repository` capable de contacter la `DB`.

`MustAsync(predicat)` : la validation réussie si le prédicat renvoie `true`

(j'enlève les autres règles par lisibilité)

```cs
public class CreateLeaveRequestDtoValidator : AbstractValidator<CreateLeaveRequestDto>
{
  private readonly
  
  public CreateLeaveRequestDtoValidator(ILeaveTypeRepository leaveTypeRepository)
  {
    RuleFor(p => p.LeaveTypeId)
      .GreaterThan(0)
      .MustAsync(async (id, token) => {
        // on peut peut être utiliser directement leaveRequestRepository
        var leaveTypeExists = await _leaveTypeRepository.Exists(id);
        
        return !leaveTypeExists; // pourquoi (!) ???
      }).WithMessage("{PropertyName} does not exist");
  }
}
```

On ajoute la méthode `Exists` à notre `IGenericRepository` :

```cs
public interface IGenericRepository<T> where T : class
{
  Task<T> Get(int id);
  Task<IReadOnlyList<T>> GetAll();
  Task<bool> Exists(int id);
  Task<T> Add(T entity);
  Task<T> Update(T entity);
  Task<T> Delete(T entity);
}
```



## Répétition de la `Validation`

Les mêmes propriété se retrouvent dans plusieurs `DTO`, elles ont donc les même `validator`dans plusieurs fichiers `*Validator.cs`.

On peut regrouper ces `Property` dans une `interface` : `ILeaveTypeDto`

```cs
public interface ILeaveTypeDto
{
  public string Name { get; set; }
  public int DefaultDays { get; set; }
}
```

ensuite les `DTO` héritent de cette `interface` :

```cs
public class CreateLeaveTypeDto : ILeaveTypeDto
{
  // ...
```
```cs

public class LeaveTypeDto : BaseDto, ILeaveTypeDto
{
  // ...
```

On peut ensuite créer son `Validator` d'près l'abstraction de cette `interface`.

Ainsi tous les `DTO` héritant de `ILeaveTypeDto` pourront bénéficier de ces `Validators`.

`ILeaveTypeDtoValidator`

```cs
using FluentValidation;

namespace Application.DTOs.LeaveType.Validators
{
  public class ILeaveTypeDtoValidator : AbstractValidator<ILeaveTypeDto>
  {
    public ILeaveTypeDtoValidator()
    {
      RuleFor(p => p.Name)
        .NotEmpty().WithMessage("{PropertyName} is required")
        .NotNull()
        .MaximumLength(50).WithMessage("{PropertyName} must not exceed 50 characters");

      RuleFor(p => p.DefaultDays)
        .NotEmpty().WithMessage("{PropertyName} is required")
        .GreaterThan(0).WithMessage("{PropertyName} must be at least 1")
        .LessThan(100).WithMessage("{PropertyName} must be less than 100");
    }
  }
}
```

Maintenant dans un `Validator` plus spécifique on pourra intégrer ces règles grâce à la méthode `Include` fournie par `FluentValidation`.

`CreateLeaveTypeDtoValidator`

```cs
public class CreateLeaveTypeDtoValidator : AbstractValidator<CreateLeaveTypeDto>
{
  public CreateLeaveTypeDtoValidator()
  {
    Include(new ILeaveTypeDtoValidator());
    
    // règles custom en plus si besoin ...
  }
}
```

On a maintenant notre `UpdateLeaveTypeDtoValidator`

```cs
public class UpdateLeaveTypeDtoValidator : AbstractValidator<LeaveTypeDto>
{
  public UpdateLeaveTypeDtoValidator()
  {
    Include(new ILeaveTypeDtoValidator());

    RuleFor(p => p.Id)
      .NotNull().WithMessage("{PropertyName} must be present");
  }
}
```

### `Include(new ILeaveTypeDto())`

C'est plus `clean`, on ne répète pas les mêmes règles, elles sont centralisées.



## Exemple avec `LeaveRequest`

### 1 On crée un `ILeaveRequestDto`

Cette `interface` récupère les propriétés commune à `CreateLeaveRequestDto` et `UpdateLeaveRequestDto`

`ILeaveRequestDto`

```cs
public interface ILeaveRequestDto
{
  public DateTime StartDate { get; set; }
  public DateTime EndDate { get; set; }
  public int LeaveTypeId { get; set; }
}
```



### 2 On crée un `Validator` pour cette `interface`

On va créer un `validator` basé sur l'abstraction de `ILeaveRequestDto`.

`ILeaveRequestDtoValidator`

```cs
public class ILeaveRequestDtoValidator : AbstractValidator<ILeaveRequestDto>
{
  private readonly ILeaveRequestRepository _leaveRequestRepository;
  public ILeaveRequestDtoValidator(ILeaveRequestRepository leaveRequestRepository)
  {
    _leaveRequestRepository = leaveRequestRepository;

    RuleFor(p => p.StartDate)
      .LessThan(p => p.EndDate).WithMessage("{PropertyName} must be before {ComparaisonValue}");

    RuleFor(p => p.LeaveTypeId)
      .MustAsync(async (id, token) => {
        var leaveTypeExists = await _leaveRequestRepository.Exists(id);
        return !leaveTypeExists;
      }).WithMessage("{PropertyName} does not exists");
  }
}
```



### 3 On ajoute l'héritage à cette interface aux `DTO` concernés

`CreateLeaveRequestDto`

```cs
public class CreateLeaveRequestDto : ILeaveRequestDto
{
```

`UpdateLeaveRequestDto`

```cs
public class UpdateLeaveRequestDto : BaseDto, ILeaveRequestDto
{
```



### 4 On crée les `validator` spécifique

Pour importer les règles commune de l'`interface` `ILeaveRequestDto`, on utilise la méthode `Include`.

Quand on instancie le `ILeaveRequestDtoValidator` on doit lui passer le `repository`.

`CreateLeaveRequestDtoValidator`

```cs
public class CreateLeaveRequestDtoValidator : AbstractValidator<CreateLeaveRequestDto>
{
  private readonly ILeaveRequestRepository _leaveRequestRepository;
        public CreateLeaveRequestDtoValidator(ILeaveRequestRepository leaveRequestRepository)
        {
            _leaveRequestRepository = leaveRequestRepository;

            Include(new ILeaveRequestDtoValidator(_leaveRequestRepository));
        }
}
```

`UpdateLeaveRequestDtoValidator`

```cs
public class UpdateLeaveRequestDtoValidator : AbstractValidator<UpdateLeaveRequestDto>
{
  private readonly ILeaveRequestRepository _leaveRequestRepository;
  public UpdateLeaveRequestDtoValidator(_ILeaveRequestRepository leaveRequestRepository)
  {
    _leaveRequestRepository = leaveRequestRepository;

    Include(new ILeaveRequestDtoValidator(_leaveRequestRepository));

    RuleFor(p => p.RequestComments)
      .NotEmpty().WithMessage("{PropertyName} is required");
  }
}
```

Ici on a une règle spécifique en plus.



### 5 On exécute la validation dans le `Handler`

`CreateLeaveRequestCommandHandler`

```cs
public async Task<int> Handle(CreateLeaveRequestCommand request, CancellationToken cancellationToken)
{
  var validator = new CreateLeaveRequestDtoValidator(_leaveRequestRepository);
  var validationResult = await validator.ValidateAsync(request.CreateLeaveRequestDto);

  if (validationResult.IsValid == false)
  {
    throw new Exception();
  }
  // ...
```



`UpdateLeaveRequestCommandHandler`

```cs
public async Task<Unit> Handle(UpdateLeaveRequestCommand request, CancellationToken cancellationToken)
{
  var validator = new UpdateLeaveRequestDtoValidator(_leaveRequestRepository);
  var validationResult = await validator.ValidateAsync(request.UpdateLeaveRequestDto);

  if(validationResult.IsValid == false)
  {
    throw new Exception();
  }
  // ...
```

















