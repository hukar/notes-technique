# Mud-01 `MudTable`

## Une simple `table`

```html
<MudTable Items="@Employees" Hover="true">
    <HeaderContent>
        <MudTh>Employee ID</MudTh>
        <MudTh>Firts Name</MudTh>
        <MudTh>Last Name</MudTh>
    </HeaderContent>
    <RowTemplate>
        <MudTd>@context.EmployeeId</MudTd>
        <MudTd>@context.FirstName</MudTd>
        <MudTd>@context.LastName</MudTd>
    </RowTemplate>
</MudTable>
```

Où `Employees` est une `List<Employee>`.

`@context` est de type `Employee`.