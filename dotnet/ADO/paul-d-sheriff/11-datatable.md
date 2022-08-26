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

