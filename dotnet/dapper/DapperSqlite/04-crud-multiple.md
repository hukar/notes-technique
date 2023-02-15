# 04 `CRUD` avec des requête `multi` tables

On peut gérer un graphe d'objet avec `QueryAsync<T1, T2, ..., TReturn>`.



## Récupérer les `Robots` avec leurs `Weapons` : `GetAllRobotsWithWeapons`

```cs
public async Task<IEnumerable<Robot>> GetAllRobotsWithWeapons()
{
    var sql = @"SELECT * 
                FROM Robot as r
                LEFT JOIN Weapon as w
                ON w.RobotId = r.Id";
```

<img src="assets/result-of-join.png" alt="result-of-join" style="zoom:50%;" />


```cs
    using var connection = _context.CreateConnection();

    var result = await connection.QueryAsync<Robot, Weapon, Robot>(sql, (robot, weapon) => {
        if(weapon is not null) robot.Weapons.Add(weapon);
        return robot;
    });

    var robots = result.GroupBy(robot => robot.Id).Select(grp => {
        var robot = grp.First();
        var weapons = grp
                        .Where(r => r.Weapons.Count > 0)
                        .Select(r =>  r.Weapons.First()).ToList();
        robot.Weapons = weapons;

        return robot;
    });

    return robots;
}
```

Ici le valeurs `null` pour `Weapon` sont gérer et s'il n'y a pas de `Weapon`, on obtient bien un tableau vide.

```json
[
  {
    "id": 1,
    "codeName": "JO-JO",
    "weapons": [ { "id": 1, "name": "Light Saber Blue" } ]
  },
  {
    "id": 2,
    "codeName": "VBG-67",
    "weapons": [ 
        			{ "id": 3, "name": "Radical Blaster Gen II" },
                 	{ "id": 2, "name": "Ultra Sword Of Fire" }
    			]
  },
  {
    "id": 3,
    "codeName": "MICH-3L",
    "weapons": []
  }
]
```

