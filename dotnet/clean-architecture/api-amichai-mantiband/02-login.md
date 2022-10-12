# 02 `Login` et `Enregistrement` : `Register`

<img src="assets/worflow-login-register-one.png" alt="worflow-login-register-one" style="zoom:50%;" />

<img src="assets/worflow-login-register-two.png" alt="worflow-login-register-two" style="zoom:50%;" />

<img src="assets/worflow-login-register-three.png" alt="worflow-login-register-three" style="zoom:50%;" />

<img src="assets/worflow-login-register-four.png" alt="worflow-login-register-four" style="zoom:50%;" />



## `BuberDinner.Contracts`

On crée le dossier `Authentication` et dedans les classes `RegisterRequest.cs`, `LoginRequest.cs` et `AuthenticationResponse.cs`

### `RegisterRequest.cs`

```cs
public record RegisterRequest(
	string FirstName,
    string LastName,
    string Email,
    string Password
);
```



### `LoginRequest.cs`

```cs
public record LoginRequest(
    string Email,
    string Password
);
```



### `AuthenticationResponse.cs`

```cs
public record AuthenticationResponse(
    Guid Id,
	string FirstName,
    string LastName,
    string Email,
    string Token
);
```

