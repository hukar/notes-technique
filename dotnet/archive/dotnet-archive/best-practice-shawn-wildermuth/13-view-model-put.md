# 13 `ViewModel` et la méthode `PUT`

```cs
[HttpPut("{id:int}")]
public async Task<ActionResult<TimeBillViewModel>> Put(int id, [FromBody] TimeBillViewModel model)
{
  var oldBill = await _ctx.TimeBills
    .Where(b => b.Id == id)
    .FirstOrDefaultAsync();

  if (oldBill == null) return BadRequest("Invalid ID");

  _mapper.Map(model, oldBill);

  var theCase = await _ctx.Cases
    .Where(c => c.Id == model.CaseId)
    .FirstOrDefaultAsync();

  var theEmployee = await _ctx.Employees
    .Where(e => e.Id == model.EmployeeId)
    .FirstOrDefaultAsync();

  oldBill.Case = theCase;
  oldBill.Employee = theEmployee;

  if (await _ctx.SaveChangesAsync() > 0)
  {
    return Ok(_mapper.Map<TimeBillViewModel>(oldBill));
  }

  return BadRequest("Failed to save new timebill");
}
```

`_mapper.Map(model, oldBill)` copie les valeurs champ par champ, cela remplace un code long du type :

```cs
oldBill.Rate = bill.Rate;
oldBill.TimeSegments = bill.TimeSegments;
oldBill.WorkDate = bill.WorkDate;
oldBill.WorkDescription = bill.WorkDescription;
```

Avec `[ApiController]` en haut de la classe la validation est active.