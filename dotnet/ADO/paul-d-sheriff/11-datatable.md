# 11 `DataTable`

## Créer un `DataTable`

```cs
DataTable dt = new DataTable();
```



## Créer une `DataColumn`

### 1. Avec la collection `dt.Columns`

```cs
dt.Columns.Add("ProductId", typeof(int));
```



### 2. Avec un objet

```cs
DataColumn dc = new() {
    DataType = typeof(string),
    ColumnName = "ProductName",
    Caption = "Product Name",
    ReadOnly = false
}
dt.Columns.Add(dc);
```

C'est plus lisible.



### 3. Directement dans la méthode `Add`

```cs
dt.Columns.Add(new DataColumn {
    DataType = typeof(decimal),
    ColumnName = "Price",
    Caption = "Price",
    ReadOnly = false
})
```



## Ajouter une ligne : `Row`

### 1. Passer les données dans `Add`

```cs
dt.Rows.Add(1, "Black Glasses", 99.98);
```



### 2. Utiliser `NewRow()`

```cs
DataRow dr = dt.NewRow();

dr["ProductId"] = 2;
dr["ProductName"] = "Marshall Hearphone";
dr["Price"] = 89.50;

dt.Rows.Add(dr);
```

Cette approche est plus lisible pour un grand groupe de données.



## Updater la `DataTable`

```cs
dt.AcceptChanges();
```



## `Clone` et `Copy`

- `Clone` crée seulement la structure (le nom et le type des colonnes).
- `Copy` crée la structure avec les données.

```cs
DataTable dtClone = dt.Clone();
DataTable dtCopy = dt.Copy();
```



## Utiliser `Select` pour filtrer notre `DataTable`

`Select` va filtrer les `Rows` et créer un tableau (`array`) de `DataRow`.

`Select` va itérer chaque `row`.

On va créer une nouvelle `DataTable` avec `ImportRow()` et appeler `AcceptChanges` quand tout est bon.

```cs
DataTable dt = productManager.GetProductAsDataTable();

DataTable dtNew = dt.Clone();

DataRow[] rows = dt.Select("Price < 100");

foreach(DataRow row in rowe)
{
    // dtNew.Rows.Add(row); => error
    
    // dtNews.Rows.Add(row.ItemArray); => Ok
    
    dtNew.ImportRow(row);
}

dtNew.AcceptChanges(); // pas nécessaire pour moi

return dtNew;
```

erreur avec `dtNew.Rows.Add(row)` :

<img src="assets/dtatrow-error-adding-sst.png" alt="dtatrow-error-adding-sst" style="zoom:50%;" />

#### Cette ligne appartient déjà à une autre table

`row.ItemArray` retourne un tableau avec les valeurs du `DataRow`.



## `CopyToDataTable`

On peut simplifier la syntaxe ci-dessus avec la méthode `CopyToDataTable` :

```cs
public static List<ProductDto> SelectDataTable(ProductManager mgr)
{
    DataTable dt = mgr.GetProductsAsDataTable();

    DataTable dtNew = dt.Select("Price < 200").CopyToDataTable();

    return TableToListTwo(dtNew);
}
```































