# Mud-04 `MudCard`

## Exemple `EmployeeCard`

```c#
@if(Employee is not null)
{
    <MudCard Class="mb-8" Style="max-width: 220px">
        <MudCardHeader>
            <CardHeaderAvatar>
                <MudAvatar Size="Size.Small" Color="Color.Secondary">@firstNameFirstLetter</MudAvatar>
            </CardHeaderAvatar>
            <CardHeaderContent>
                <MudText Typo="Typo.h6">
                    @Employee.FirstName @Employee.LastName
                </MudText>
            </CardHeaderContent>
        </MudCardHeader>
        <MudCardMedia Image="@imagePath" Style="width: 140px; margin: auto" Height="140" Class="rounded-circle"/>
        <MudCardContent>           
            <MudText Typo="Typo.body2">The best friend for working.</MudText>
        </MudCardContent>
        <MudCardActions>
            <MudButton StartIcon="@Icons.Material.Filled.Face5" IconClass="pink-text text-accent-3" Variant="Variant.Text" Color="Color.Primary">Employee Details</MudButton>
        </MudCardActions>
    </MudCard>
}

@code {
    string? imagePath;
    string? firstNameFirstLetter;

    [Parameter]
    public Employee? Employee { get; set; }

    protected override void OnInitialized()
    {
        if(Employee is not null)
        {
            imagePath = $"https://gill/person/{Employee.EmployeeId}.jpg";

            firstNameFirstLetter = Employee.FirstName[0].ToString();
        }
    }
}
```

Et dans le `parent component`:

```c#
<div class="d-flex gap-4 mt-8">
    @foreach(var employee in Employees)
    {
        <EmployeeCard Employee="employee" />
    }
</div>
```

<img src="assets/employee-card-exemple-mud-blazor.png" alt="employee-card-exemple-mud-blazor" />