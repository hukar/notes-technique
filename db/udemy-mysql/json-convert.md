# Conversion des données en `JSON`

```sql
SELECT json_pretty(json_object('id', `customer_id`,'name', CONCAT(`first_name`, " ", `last_name`) )) 
AS 'JSON' 
FROM customers
```

`json_pretty` pour avoir une belle indentation

`json_object` pour avoir un object `JSON` en sortie

`CONCAT` pour réunir deux champs.

