# 07 Infrastructure : 

# exemple avec un service  d'email

`Infrastrucure` est le projet où vont se trouver tous les services des parties tierces (third party services).



## Refactor

On va inverser les dossier `Contracts` et `Persitence` du projet application pour avoir

`Contracts`

​		|_`Persistence`

et non plus :

`Persistence`

​		|_`Contracts`

On doit aussi corriger les `namespaces`.



## Création de l'interface `IEmailSender`

Dans le dossier `Contracts` du projet `Application`, on va créer un nouveau dossier `Infrastructure` et dedans notre interface `IEmailSender`

```cs
using System.Threading.Tasks;
using Application.Models;

namespace Application.Contracts.Infrastructure
{
    public interface IEmailSender
    {
         Task<bool> SendEmail(Email email);
    }
}
```



Toujours dans le projet `Application` on va créer un dossier `Models` et dedans la classe `Email`

```cs
namespace Application.Models
{
    public class Email
    {
        public string To { get; set; }
        public string Subject { get; set; }
        public string Body { get; set; }
    }
}
```

On ajoute aussi la classe `EmailSettings`

```cs
namespace Application.Models
{
    public class EmailSettings
    {
        public string ApiKey { get; set; }
        public string FromAddress { get; set; }
        public string FromName { get; set; }
    }
}
```



## Utilisation dans un `handler` : `CreateLeaveRequestHandler`

On doit injecter noter service dans le constructeur :

```cs
private readonly ILeaveRequestRepository _leaveRequestRepository;
private readonly IMapper _mapper;
private readonly IEmailSender _emailSender;
public CreateLeaveRequestCommandHandler(ILeaveRequestRepository leaveRequestRepository, IMapper mapper, IEmailSender emailSender)
{
  _emailSender = emailSender;
  _mapper = mapper;
  _leaveRequestRepository = leaveRequestRepository;

}
```

Dans la méthode `Handle` une fois que tout a été fait :

```cs
public async Task<BaseCommandResponse> Handle(CreateLeaveRequestCommand request, CancellationToken cancellationToken)
{
  // ...

  response.Id = leaveRequest.Id;
  response.Message = "Creation successful";
  response.Success = true;
  
  var email = new Email {
    To = "k.meshoub@gmail.com",
    Body = $"Your leave request for {request.LeaveRequestDto.StartDate:D} to {request.LeaveRequestDto.EndDate} has been submitted successfully",
    Subject = "Leave Request Submitted"
  };
  
  try
  {
    await _emailSender.SendEmail(email);
  }
  catch(Exception ex)
  {
    // Log or Handle error but don't throw
  }

  return response;
}
```

`{StartDate:D}` permet de formatter la date avec les noms longs.

On `catch` l'`Exception` mais on n'en relance pas de nouvelle, il ne faut pas que l'application crash parcequ'un email n'est pas envoyé.



## Implementation de `EmailSender`

Création du projet `Infrastructure`

```bash
dotnet new classlib -o Infrastructure

dotnet sln add Infrastructure

dotnet add Infrastructure reference Application
```

### Package `Nuget`

Il faut installer `ConfigurationExtension` (pourquoi ?, et aussi dans `Persistence`) et `SendGrid` :

```bash
dotnet add package Microsoft.Extensions.Options.ConfigurationExtensions --version 5.0.0
```

```bash
dotnet add package SendGrid --version 9.24.2
```



Dans le projet `Infrastructure`, dans le dossier `Mail`, on va créer une classe `EmailSender` :

```cs
namespace Infrastructure.Mail
{
  public class EmailSender : IEmailSender
  {
    private EmailSettings _emailSettings { get; }

    public EmailSender(IOptions<EmailSettings> emailsettings)
    {
      _emailSettings = emailSettings;
    }

    public async Task<bool> SendEmail(Email email)
    {
      var client = new SendGridClient(_emailSettings.ApiKey);
      var to = new EmailAddress(email.To);
      var from = new EmailAddress
      {
        Email = _emailSettings.FromAddress,
        Name = _emailSettings.FromName
      }
      
      var message = MailHelper.CreateSingleEmail(from, to, email.Subject, email.Body, email.Body);
      var response = await client.SendEmailAsync(message);
      
      return response.StatusCode == HttpStatusCode.OK || response.StatusCode == HttpStatusCode.Accepted;
    }
  }
}
```

`IOptions<EmailSettings>` permet de placer les données de configuration dans `appsettings.json` et de remplir avec un objet de type `EmailSettings`. La configuration reste découplée et centralisée dans `appsettings.json`.

`IOptions` fait partie du package `Microsoft.Extensions.Options.ConfigurationExtensions`.



## `InfrastructureServicesRegistration`

De nouveau on crée une méthode d'extension pour gérer l'injection de dépendance dans le `Startup` du projet `exe`.

```cs
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace Infrastructure
{
    public static class InfrastructureServicesRegistration
    {
        public static IServiceCollection AddInfrastructureServices(this IServiceCollection services, IConfiguration configuration)
        {
            services.Configure<EmailSettings>(configuration.GetSection("EmailSettings"));
          services.AddTransient<IEmailSender, EmailSender>();

            return services;
        }
    }
}
```

Le service d'`email` est défini dans la couche `Application` : `Contracts/Infrastructure` et implémenté dans `Infrastructure`.

`Interface` pour la logique métier dans `Application`.

Implémentation dans une classe dans la couche `Infrastructure`.



















