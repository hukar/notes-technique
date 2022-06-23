# 05 `Response Object`



## `Custom Exception`

On va créer ses propres `exception` dans un dossier `Exceptions` à la racine de `Application`.

On va créer trois nouvelles `exception` :

- `BadRequestException`
- `NotFoundException`
- `ValidationException`

`BadRequestException`

```cs
using System;

namespace Application.Exceptions
{
    public class BadRequestException : ApplicationException
    {
        public BadRequestException(string message) : base(message)
        {
          
        }
    }
}
```

`NotFoundException`

```cs
public class NotFoundException : ApplicationException
{
  public NotFoundException(string name, object key) : base($"{name} ({key}) was not found")
  {

  }
}
```

`ValidationException`

```cs
public class ValidationException : ApplicationException
{
	public List<string> Errors { get; set; } = new();
  
  public ValidationException(ValidationResult validationResult)
  {
    foreach(var error in validationResult.Errors)
    {
      Errors.Add(error.ErrorMessage);
    }
  }
}
```



## Utilisation de `ValidationException`

On retrouve cette `exception` dans les `handler` en `create` et `update` qui doivent valider des données en entrée.

On va maintenant pouvoir utiliser notre `ValidationException` dans notre `handler` :

`UpdateLeaveTypeCommandHandler`

```cs
public async Task<Unit> Handle(UpdateLeaveTypeCommand request, CancellationToken cancellationToken)
{
  var validator = new UpdateLeaveTypeDtoValidator();
  var validatorResult = await validator.ValidateAsync(request.LeaveTypeDto);

  if (validatorResult.IsValid == false)
  {
    throw new ValidationException(validationResult);
  }
```



## Utilisation de `NotFoundException`

Dans les `handler` en `delete` on utilise ce type d'`exception`.

`DeleteLeaveTypeCommandHandler`

```cs
public async Task<Unit> Handle(DeleteLeaveTypeCommand request, CancellationToken cancellationToken)
{
  var leaveType = await _leaveTypeRepository.Get(request.Id);

  if(leaveType == null)
    throw new NotFoundException(nameof(LeaveType), request.Id);
```





## `Custom Response`

Plutôt que de lancer une `exception` on veut retourner au `client` les informations d'erreur et de réussite.

On va créer un dossier `Responses` à la racine du projet `Application` et créer une classe `BaseCommandResponse`

```cs
public class BaseCommandResponse
{
  public int Id { get; set; }
  public bool Success { get; set; }
  public string Message { get; set; }
  public List<string> Errors { get; set; }
}
```

On va maintenant utiliser notre `BaseCommandResponse` dans un `handler`.

`CreateLeaveRequestCommandHandler`

```cs
public class CreateLeaveRequestCommandHandler : IRequestHandler<CreateLeaveRequestCommand, BaseCommandResponse> // <= change type
{
  // ...

  public async Task<BaseCommandResponse> Handle(CreateLeaveRequestCommand request, CancellationToken cancellationToken)
  {
    var response = new BaseCommandResponse(); // <= instancie

    var validator = new CreateLeaveRequestDtoValidator(_leaveRequestRepository);
    var validationResult = await validator.ValidateAsync(request.CreateLeaveRequestDto);

    // on remplie notre objet response
    if (validationResult.IsValid == false)
    {
      response.Success = false;
      response.Message = "Creation failed";
      response.Errors = validationResult.Errors.Select(e => e.ErrorMessage).ToList();
    }

    var leaveRequest = _mapper.Map<Domain.LeaveRequest>(request.CreateLeaveRequestDto);

    leaveRequest = await _leaveRequestRepository.Add(leaveRequest);

    response.Id = leaveRequest.Id;
    response.Message = "Creation successful";
    response.Success = true;

    return response;
  }
```

Il faut aussi changer le type de retour de la `IRequest` : `CreateLeaveRequestCommand`

```cs
public class CreateLeaveRequestCommand : IRequest<BaseCommandResponse>
{
```

au lieu de 

```cs
public class CreateLeaveRequestCommand : IRequest<int>
{
```





































