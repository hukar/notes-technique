# CC Les mots clés en Programmation Objet

## `virtual` : `Property`

Désigner une propriété comme `virtual` signifie que celle-ci pourra être `override`.

C'est à dire redéfinit, réécrite par une classe héritière.

```cs
class BaseClass
{
  public virtual string Name { get; set; }
}
```

```cs
class DerivedClass : BaseClass
{
  private string _name;
  public override string Name
  {
    get
    {
      return _name;
    }
    set
    {
      if(!string.IsNullOrEmpty(value))
      {
        _name = value;
      }
      else
      {
        _name = "unknow";
      }
    }
  }
}
```

