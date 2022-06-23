# 09 `Mediatr` pour des `controller` ultra-fin



## Installer `Mediatr`

On va d'abord installer le package pour `Mediatr`  : utilisation de l'extension `Nuget Package Manager` pour `VSCode`.

On ne prend pas ici la version `Dependency Injection` mais juste le package de base.



## Création du controller

On va utiliser `apsnet-codegenerator` :

installation du package `Microsoft.VisualStudio.Web.CodeGeneration.Design`

```bash
dotnet aspnet-codegenerator controller -name LeaveTypes -api -outDir Controllers -actions
```

`-actions` ajoute des `actions` de base.

`LeaveTypesController`

```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;

namespace API.Controllers
{
  [Route("api/[controller]")]
  [ApiController]
  public class LeaveTypesController : ControllerBase
  {
    private readonly IMediator _mediator;

    public LeaveTypesController(IMediator mediator)
    {
      _mediator = mediator;
    } 
```



```cs
// GET: api/LeaveTypes
[HttpGet]
public async Task<ActionResult<List<LeaveTypeDto>>> Get(new GetLeaveTypeListRequest())
{
  var leaveTypes = await _mediator.Send(new GetLeaveTypeListRequest());
  return Ok(leaveTypes);
}
```

ou plus court encore :

```cs
[HttpGet]
public async Task<ActionResult<List<LeaveTypeDto>>> Get() 
  => Ok(await _mediator.Send(new GetLeaveTypeListRequest()));
```



```cs
// GET: api/LeaveTypes/5
[HttpGet("{id}", Name = "Get")]
public string Get(int id) 
  => Ok(_mediator.Send(new GetLeaveTypeDetailRequest { Id = id }));
```



```cs
// POST: api/LeaveTypes
[HttpPost]
public async Task<ActionResult<int>> Post([FromBody] CreateLeaveTypeDto leaveType)
  => Ok(await _mediator.Send(new CreateLeaveTypeCommand { CreateLeaveTypeDto = leaveType }));
```



```cs
// PUT: api/LeaveTypes
[HttpPut]
public async Task<ActionResult> Put([FromBody] LeaveTypeDto leaveType)
{
  var command = new UpdateLeaveTypeCommand { LeaveTypeDto = leaveType };
  await _mediator.Send(command);
  
  return NoContent();
}
```



```cs
// DELETE: api/LeaveTypes/5
[HttpDelete("{id}")]
public async Task<ActionResult> Delete(int id)
{
  var command = new DeleteLeaveTypeCommand { Id = id };
  await _mediator.Send(command);
  
  return NoContent();
}
```



## Exercice : `LeaveRequestController`

```cs
[Route("api/[controller]")]
[ApiController]
public class LeaveRequestController : ControllerBase
{
  private readonly IMediator _mediator;
  public LeaveRequestController(IMediator mediator)
  {
    _mediator = mediator;

  }

  // GET: api/LeaveRequest
  [HttpGet]
  public async Task<ActionResult<List<LeaveRequestListDto>>> Get() => Ok(await _mediator.Send(new GetLeaveRequestListRequest()));

  // GET: api/LeaveRequest/5
  [HttpGet("{id}", Name = "Get")]
  public async Task<ActionResult<LeaveRequestDto>> Get(int id) => Ok(await _mediator.Send(new GetLeaveRequestDetailRequest { Id = id }));

  // POST: api/LeaveRequest
  [HttpPost]
  public async Task<ActionResult<BaseCommandResponse>> Post([FromBody] CreateLeaveRequestDto leaveRequest)
  {
    var response = await _mediator.Send(new CreateLeaveRequestCommand { CreateLeaveRequestDto = leaveRequest });

    return Ok(response);
  }

  // PUT: api/LeaveRequest/5
  [HttpPut("{id}")]
  public async Task<ActionResult> Put(int id, [FromBody] UpdateLeaveRequestDto leaveRequest)
  {
    await _mediator.Send(new UpdateLeaveRequestCommand { UpdateLeaveRequestDto = leaveRequest });

    return NoContent();
  }

  // PUT: api/LeaveRequest/ChangeApproval/5
  [HttpPut("ChangeApproval/{id}")]
  public async Task<ActionResult> Put(int id, [FromBody] ChangeLeaveRequestApprovalDto changeApproval)
  {
    await _mediator.Send(new ChangeLeaveRequestApprovalCommand { ChangeLeaveRequestApprovalDto = changeApproval });

    return NoContent();
  }
}
```

 

## Exercice : `LeaveAllocationController`

```cs
// GET: api/LeaveAllocation
[HttpGet]
public async Task<ActionResult<List<LeaveAllocationDto>>> Get() => Ok(await _mediator.Send(new GetLeaveAllocationListRequest()));

// GET: api/LeaveAllocation/5
[HttpGet("{id}", Name = "Get")]
public async Task<ActionResult<LeaveAllocationDto>> Get(int id) => Ok(await _mediator.Send(new GetLeaveAllocationDetailRequest { Id = id }));

// POST: api/LeaveAllocation
[HttpPost]
public async Task<ActionResult<int>> Post([FromBody] CreateLeaveAllocationDto leaveAllocation) => Ok(await _mediator.Send(new CreateLeaveAllocationCommand { CreateLeaveAllocationDto = leaveAllocation }));

// PUT: api/LeaveAllocation
[HttpPut]
public async Task<ActionResult> Put([FromBody] UpdateLeaveAllocationDto leaveAllocation)
{
  await _mediator.Send(new UpdateLeaveAllocationCommand { UpdateLeaveAllocationDto = leaveAllocation });

  return NoContent();
}
```

